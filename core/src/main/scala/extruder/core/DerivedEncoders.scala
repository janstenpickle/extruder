package extruder.core

import shapeless._
import shapeless.labelled.FieldType

import scala.reflect.runtime.universe.TypeTag

trait DerivedEncoders { self: Encoders with EncodeTypes =>
  implicit def cnilEncoder[F[_]](implicit F: ExtruderEffect[F]): Enc[F, CNil] = mkEncoder { (_, _) =>
    F.validationFailure(s"Impossible!")
  }

  implicit def cconsEncoder[F[_], K <: Symbol, H, T <: Coproduct](
    implicit key: Witness.Aux[K],
    headEncode: Enc[F, H],
    tailEncode: Lazy[Enc[F, T]],
    typeEncode: Lazy[Enc[F, String]],
    hints: Hint,
    F: ExtruderEffect[F]
  ): Enc[F, FieldType[K, H] :+: T] =
    mkEncoder { (path, value) =>
      val chooseEncoder: F[EncodeData] = value match {
        case Inl(h) => headEncode.write(path, h)
        case Inr(t) => tailEncode.value.write(path, t)
      }

      F.ap2(F.pure[(EncodeData, EncodeData) => EncodeData](monoid.combine))(
        typeEncode.value.write(hints.pathWithType(path), key.value.name),
        chooseEncoder
      )
    }

  implicit def unionEncoder[F[_], T, O <: Coproduct](
    implicit gen: LabelledGeneric.Aux[T, O],
    underlying: Lazy[Enc[F, O]],
    F: ExtruderEffect[F],
    lp: LowPriority
  ): Enc[F, T] =
    mkEncoder((path, value) => underlying.value.write(path, gen.to(value)))

  trait DerivedEncoder[T, F[_], Repr <: HList] {
    def write(path: List[String], value: Repr): F[EncodeData]
  }

  implicit def hNilDerivedEncoder[T, F[_]](implicit F: ExtruderEffect[F]): DerivedEncoder[T, F, HNil] =
    new DerivedEncoder[T, F, HNil] {
      override def write(path: List[String], value: HNil): F[EncodeData] = F.pure(monoid.empty)
    }

  implicit def hConsDerivedEncoder[T, F[_], K <: Symbol, V, TailRepr <: HList](
    implicit key: Witness.Aux[K],
    F: ExtruderEffect[F],
    encoder: Lazy[Enc[F, V]],
    tailEncoder: Lazy[DerivedEncoder[T, F, TailRepr]]
  ): DerivedEncoder[T, F, FieldType[K, V] :: TailRepr] =
    new DerivedEncoder[T, F, FieldType[K, V] :: TailRepr] {
      override def write(path: List[String], value: FieldType[K, V] :: TailRepr): F[EncodeData] = {
        val fieldName = key.value.name

        F.ap2(F.pure[(EncodeData, EncodeData) => EncodeData](monoid.combine))(
          encoder.value.write(path :+ fieldName, value.head),
          tailEncoder.value.write(path, value.tail)
        )
      }
    }

  implicit def productEncoder[F[_], T, GenRepr <: HList](
    implicit gen: LabelledGeneric.Aux[T, GenRepr],
    tag: TypeTag[T],
    F: ExtruderEffect[F],
    encoder: Lazy[DerivedEncoder[T, F, GenRepr]]
  ): Enc[F, T] = {
    lazy val className: String = tag.tpe.typeSymbol.name.toString
    mkEncoder[F, T] { (path, value) =>
      encoder.value.write(path :+ className, gen.to(value))
    }
  }
}
