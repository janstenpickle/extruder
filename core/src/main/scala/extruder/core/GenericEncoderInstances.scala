package extruder.core

import cats.Applicative
import cats.kernel.Monoid
import extruder.data.PathElement
import shapeless._
import shapeless.labelled.FieldType

import scala.reflect.ClassTag

trait GenericEncoderInstances {
  implicit def cnilEncoder[F[_], S, O]: Encoder[F, S, CNil, O] = Encoder.make { (_, _, _) =>
    sys.error("Impossible!")
  }

  implicit def cconsEncoder[F[_], K <: Symbol, H, T <: Coproduct, S, D](
    implicit key: Witness.Aux[K],
    headEncode: Encoder[F, S, H, D],
    tailEncode: Lazy[Encoder[F, S, T, D]],
    typeEncode: Lazy[Encoder[F, S, String, D]],
    monoid: Monoid[D],
    F: Applicative[F]
  ): Encoder[F, S, FieldType[K, H] :+: T, D] =
    Encoder.make[F, S, FieldType[K, H] :+: T, D] { (path, settings, value) =>
      val chooseEncoder: F[D] = value match {
        case Inl(h) => headEncode.write(path, settings, h)
        case Inr(t) => tailEncode.value.write(path, settings, t)
      }

      F.ap2(F.pure[(D, D) => D](monoid.combine))(
        typeEncode.value.write(path :+ PathElement.Type, settings, key.value.name),
        chooseEncoder
      )
    }

  implicit def unionEncoder[F[_], T, O <: Coproduct, S, D](
    implicit gen: LabelledGeneric.Aux[T, O],
    underlying: Lazy[Encoder[F, S, O, D]],
    lp: LowPriority,
    refute: Refute[EncoderRefute[T, S, D]],
    refuteShow: Refute[Show[T]],
    refuteMultiShow: Refute[MultiShow[T]],
    neOpt: T <:!< Option[_],
    neCol: T <:!< TraversableOnce[_]
  ): Encoder[F, S, T, D] =
    Encoder.make[F, S, T, D]((path, settings, value) => underlying.value.write(path, settings, gen.to(value)))

  private[core] trait DerivedEncoder[F[_], T, Repr <: HList, S, D] {
    def write(path: List[PathElement], settings: S, value: Repr): F[D]
  }

  implicit def hNilDerivedEncoder[F[_], T, S, D](
    implicit F: Applicative[F],
    monoid: Monoid[D]
  ): DerivedEncoder[F, T, HNil, S, D] =
    new DerivedEncoder[F, T, HNil, S, D] {
      override def write(path: List[PathElement], settings: S, value: HNil): F[D] = F.pure(monoid.empty)
    }

  implicit def hConsDerivedEncoder[F[_], T, K <: Symbol, V, TailRepr <: HList, S, D](
    implicit key: Witness.Aux[K],
    F: Applicative[F],
    encoder: Lazy[Encoder[F, S, V, D]],
    monoid: Monoid[D],
    tailEncoder: Lazy[DerivedEncoder[F, T, TailRepr, S, D]]
  ): DerivedEncoder[F, T, FieldType[K, V] :: TailRepr, S, D] =
    new DerivedEncoder[F, T, FieldType[K, V] :: TailRepr, S, D] {
      override def write(path: List[PathElement], settings: S, value: FieldType[K, V] :: TailRepr): F[D] = {
        val fieldName = key.value.name

        F.ap2(F.pure[(D, D) => D](monoid.combine))(
          encoder.value.write(path :+ PathElement.Standard(fieldName), settings, value.head),
          tailEncoder.value.write(path, settings, value.tail)
        )
      }
    }

  implicit def productEncoder[F[_], T, GenRepr <: HList, S, D](
    implicit gen: LabelledGeneric.Aux[T, GenRepr],
    tag: ClassTag[T],
    encoder: Lazy[DerivedEncoder[F, T, GenRepr, S, D]],
    lp: LowPriority,
    refute: Refute[EncoderRefute[T, S, D]],
    refuteShow: Refute[Show[T]],
    refuteMultiShow: Refute[MultiShow[T]]
  ): Encoder[F, S, T, D] =
    Encoder.make[F, S, T, D] { (path, settings, value) =>
      val newPath = path :+ PathElement.ClassName(tag.runtimeClass.getSimpleName)
      encoder.value.write(newPath, settings, gen.to(value))
    }
}
