package extruder.core

import cats.Applicative
import cats.kernel.Monoid
import shapeless._
import shapeless.labelled.FieldType

import scala.reflect.runtime.universe.TypeTag

trait GenericEncoderTInstances {
  implicit def cnilEncoder[F[_], S, O]: EncoderT[F, S, CNil, O] = EncoderT.make { (_, _, _) =>
    sys.error("Impossible!")
  }

  implicit def cconsEncoder[F[_], K <: Symbol, H, T <: Coproduct, S <: Settings, D](
    implicit key: Witness.Aux[K],
    headEncode: EncoderT[F, S, H, D],
    tailEncode: Lazy[EncoderT[F, S, T, D]],
    typeEncode: Lazy[EncoderT[F, S, String, D]],
    monoid: Monoid[D],
    F: Applicative[F]
  ): EncoderT[F, S, FieldType[K, H] :+: T, D] =
    EncoderT.make[F, S, FieldType[K, H] :+: T, D] { (path, settings, value) =>
      val chooseEncoder: F[D] = value match {
        case Inl(h) => headEncode.write(path, settings, h)
        case Inr(t) => tailEncode.value.write(path, settings, t)
      }

      F.ap2(F.pure[(D, D) => D](monoid.combine))(
        typeEncode.value.write(settings.pathWithType(path), settings, key.value.name),
        chooseEncoder
      )
    }

  implicit def unionEncoder[F[_], T, O <: Coproduct, S, D](
    implicit gen: LabelledGeneric.Aux[T, O],
    underlying: Lazy[EncoderT[F, S, O, D]],
    lp: LowPriority,
    refute: Refute[EncoderTRefute[T, S, D]],
    refuteShow: Refute[Show[T]],
    refuteMultiShow: Refute[MultiShow[T]],
    neOpt: T <:!< Option[_],
    neCol: T <:!< TraversableOnce[_]
  ): EncoderT[F, S, T, D] =
    EncoderT.make[F, S, T, D]((path, settings, value) => underlying.value.write(path, settings, gen.to(value)))

  private[core] trait DerivedEncoder[F[_], T, Repr <: HList, S, D] {
    def write(path: List[String], settings: S, value: Repr): F[D]
  }

  implicit def hNilDerivedEncoder[F[_], T, S, D](
    implicit F: Applicative[F],
    monoid: Monoid[D]
  ): DerivedEncoder[F, T, HNil, S, D] =
    new DerivedEncoder[F, T, HNil, S, D] {
      override def write(path: List[String], settings: S, value: HNil): F[D] = F.pure(monoid.empty)
    }

  implicit def hConsDerivedEncoder[F[_], T, K <: Symbol, V, TailRepr <: HList, S, D](
    implicit key: Witness.Aux[K],
    F: Applicative[F],
    encoder: Lazy[EncoderT[F, S, V, D]],
    monoid: Monoid[D],
    tailEncoder: Lazy[DerivedEncoder[F, T, TailRepr, S, D]]
  ): DerivedEncoder[F, T, FieldType[K, V] :: TailRepr, S, D] =
    new DerivedEncoder[F, T, FieldType[K, V] :: TailRepr, S, D] {
      override def write(path: List[String], settings: S, value: FieldType[K, V] :: TailRepr): F[D] = {
        val fieldName = key.value.name

        F.ap2(F.pure[(D, D) => D](monoid.combine))(
          encoder.value.write(path :+ fieldName, settings, value.head),
          tailEncoder.value.write(path, settings, value.tail)
        )
      }
    }

  implicit def productEncoder[F[_], T, GenRepr <: HList, S <: Settings, D](
    implicit gen: LabelledGeneric.Aux[T, GenRepr],
    tag: TypeTag[T],
    encoder: Lazy[DerivedEncoder[F, T, GenRepr, S, D]],
    lp: LowPriority,
    refute: Refute[EncoderTRefute[T, S, D]],
    refuteShow: Refute[Show[T]],
    refuteMultiShow: Refute[MultiShow[T]]
  ): EncoderT[F, S, T, D] = {
    lazy val className: String = tag.tpe.typeSymbol.name.toString
    EncoderT.make[F, S, T, D] { (path, settings, value) =>
      val newPath =
        if (settings.includeClassNameInPath) path :+ className
        else path
      encoder.value.write(newPath, settings, gen.to(value))
    }
  }
}
