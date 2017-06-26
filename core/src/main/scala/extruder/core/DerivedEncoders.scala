package extruder.core

import cats.effect.IO
import cats.syntax.cartesian._
import shapeless._
import shapeless.labelled.FieldType

import scala.reflect.runtime.universe.TypeTag

trait DerivedEncoders { self: Encoders with EncodeTypes =>
  implicit def cnilEncoder[F[_], E](implicit AE: ExtruderApplicativeError[F, E]): Enc[F, CNil] = mkEncoder((_, _) =>
    IO.pure(AE.validationFailure(s"Impossible!"))
  )

  implicit def cconsEncoder[F[_], E, K <: Symbol, H, T <: Coproduct](implicit key: Witness.Aux[K],
                                                                     headEncode: Enc[F, H],
                                                                     tailEncode: Lazy[Enc[F, T]],
                                                                     typeEncode: Lazy[Enc[F, String]],
                                                                     utils: Hint,
                                                                     AE: ExtruderApplicativeError[F, E]): Enc[F, FieldType[K, H] :+: T] =
  mkEncoder { (path, value) =>
    val chooseEncoder: IO[F[EncodeConfig]] = value match {
      case Inl(h) => headEncode.write(path, h)
      case Inr(t) => tailEncode.value.write(path, t)
    }

    for {
      tpe <- typeEncode.value.write(utils.pathWithType(path), key.value.name)
      v <- chooseEncoder
    } yield (tpe |@| v).map(monoid.combine)
  }

  implicit def unionEncoder[F[_], E, T, O <: Coproduct](implicit gen: LabelledGeneric.Aux[T, O],
                                                        underlying: Lazy[Enc[F, O]],
                                                        AE: ExtruderApplicativeError[F, E],
                                                        lp: LowPriority): Enc[F, T] =
    mkEncoder((path, value) => underlying.value.write(path, gen.to(value)))


  trait DerivedEncoder[T, F[_], Repr <: HList] {
    def write(path: Seq[String], value: Repr): IO[F[EncodeConfig]]
  }

  implicit def hNilDerivedEncoder[T, F[_], E](implicit AE: ExtruderApplicativeError[F, E]): DerivedEncoder[T, F, HNil] =
    new DerivedEncoder[T, F, HNil] {
      override def write(path: Seq[String], value: HNil): IO[F[EncodeConfig]] = IO(AE.pure(monoid.empty))
    }

  implicit def hConsDerivedEncoder[T, F[_], E, K <: Symbol, V, TailRepr <: HList](implicit key: Witness.Aux[K],
                                                                                  AE: ExtruderApplicativeError[F, E],
                                                                                  encoder: Lazy[Enc[F, V]],
                                                                                  tailEncoder: Lazy[DerivedEncoder[T, F, TailRepr]]): DerivedEncoder[T, F, FieldType[K, V] :: TailRepr] =
     new DerivedEncoder[T, F, FieldType[K, V] :: TailRepr] {
       override def write(path: Seq[String], value: FieldType[K, V] :: TailRepr): IO[F[EncodeConfig]] = {
         val fieldName = key.value.name

         for {
           head <- encoder.value.write(path :+ fieldName, value.head)
           tail <- tailEncoder.value.write(path, value.tail)
         } yield (head |@| tail).map(monoid.combine)
       }
     }

  implicit def productEncoder[F[_], E, T, GenRepr <: HList](implicit gen: LabelledGeneric.Aux[T, GenRepr],
                                                            tag: TypeTag[T],
                                                            AE: ExtruderApplicativeError[F, E],
                                                            encoder: Lazy[DerivedEncoder[T, F, GenRepr]]): Enc[F, T] = {
    lazy val className: String = tag.tpe.typeSymbol.name.toString
    mkEncoder[F, T]((path, value) =>
      encoder.value.write(path :+ className, gen.to(value))
    )
  }

}
