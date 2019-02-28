package extruder.core

import cats.{Applicative, Functor, Monad}
import cats.implicits._
import extruder.data.PathElement
import shapeless._
import shapeless.labelled.{field, FieldType}

import scala.reflect.runtime.universe.TypeTag

trait GenericDecoderInstances {
  private[core] trait DerivedDecoder[F[_], Repr <: Coproduct, S, I] {
    def read(path: List[PathElement], settings: S, data: I): F[Repr]
  }

  implicit def cnilDecoderWithSettings[F[_]: Functor, S <: Settings, I](
    implicit error: ExtruderErrors[F]
  ): DerivedDecoder[F, CNil, S, I] =
    new DerivedDecoder[F, CNil, S, I] {
      override def read(path: List[PathElement], settings: S, data: I): F[CNil] =
        error.validationFailure(
          s"Could not find specified implementation of sealed type at path '${settings.pathElementListToString(path :+ PathElement.Type)}'"
        )
    }

  implicit def cnilDecoder[F[_]: Functor, S, I](
    implicit error: ExtruderErrors[F],
    lp: LowPriority
  ): DerivedDecoder[F, CNil, S, I] =
    new DerivedDecoder[F, CNil, S, I] {
      override def read(path: List[PathElement], settings: S, data: I): F[CNil] =
        error.validationFailure(s"Could not find specified implementation of sealed type")
    }

  implicit def cconsDecoder[F[_], K <: Symbol, H, Repr <: Coproduct, S, I](
    implicit key: Witness.Aux[K],
    headResolve: Lazy[Decoder[F, S, H, I]],
    tailResolve: Lazy[DerivedDecoder[F, Repr, S, I]],
    typeResolver: Lazy[Decoder[F, S, Option[String], I]],
    F: Monad[F],
    errors: ExtruderErrors[F]
  ): DerivedDecoder[F, FieldType[K, H] :+: Repr, S, I] = new DerivedDecoder[F, FieldType[K, H] :+: Repr, S, I] {
    override def read(path: List[PathElement], settings: S, data: I): F[FieldType[K, H] :+: Repr] = {
      def head: F[FieldType[K, H] :+: Repr] =
        headResolve.value.read(path, settings, None, data).map(v => Inl(field[K](v)))

      def tail: F[FieldType[K, H] :+: Repr] = tailResolve.value.read(path, settings, data).map(Inr(_))

      val onValidType: Option[String] => F[FieldType[K, H] :+: Repr] = {
        case None => errors.fallback(head)(tail)
        case Some(tpe) if tpe == key.value.name => head
        case _ => tail
      }
      F.flatMap(typeResolver.value.read(path :+ PathElement.Type, settings, None, data))(onValidType)
    }
  }

  implicit def unionDecoder[F[_], T, V <: Coproduct, S, I](
    implicit gen: LabelledGeneric.Aux[T, V],
    underlying: Lazy[DerivedDecoder[F, V, S, I]],
    F: Functor[F],
    refute: Refute[DecoderRefute[T, S, I]],
    refuteParser: Refute[Parser[T]],
    refuteMultiParser: Refute[MultiParser[F, T]],
    neOpt: T <:!< Option[_],
    neCol: T <:!< TraversableOnce[_]
  ): Decoder[F, S, T, I] =
    Decoder.make[F, S, T, I] { (path, settings, _, data) =>
      underlying.value.read(path, settings, data).map(gen.from)
    }

  private[core] trait DerivedDecoderWithDefault[F[_], T, Repr <: HList, DefaultRepr <: HList, S, I] {
    def read(path: List[PathElement], settings: S, default: DefaultRepr, data: I): F[Repr]
  }

  implicit def hNilDerivedDecoder[F[_], T, S, D](
    implicit F: Applicative[F]
  ): DerivedDecoderWithDefault[F, T, HNil, HNil, S, D] =
    new DerivedDecoderWithDefault[F, T, HNil, HNil, S, D] {
      override def read(path: List[PathElement], settings: S, default: HNil, data: D): F[HNil] =
        F.pure(HNil)
    }

  implicit def hConsDerivedDecoder[T, F[_], K <: Symbol, V, TailRepr <: HList, DefaultsTailRepr <: HList, S, I](
    implicit key: Witness.Aux[K],
    F: Applicative[F],
    decoder: Lazy[Decoder[F, S, V, I]],
    tailDecoder: Lazy[DerivedDecoderWithDefault[F, T, TailRepr, DefaultsTailRepr, S, I]]
  ): DerivedDecoderWithDefault[F, T, FieldType[K, V] :: TailRepr, Option[V] :: DefaultsTailRepr, S, I] =
    new DerivedDecoderWithDefault[F, T, FieldType[K, V] :: TailRepr, Option[V] :: DefaultsTailRepr, S, I] {
      override def read(
        path: List[PathElement],
        settings: S,
        default: Option[V] :: DefaultsTailRepr,
        data: I
      ): F[::[FieldType[K, V], TailRepr]] = {
        val fieldName = key.value.name

        F.ap2(F.pure((h: V, t: TailRepr) => field[K](h) :: t))(
          decoder.value.read(path :+ PathElement.Standard(fieldName), settings, default.head, data),
          tailDecoder.value.read(path, settings, default.tail, data)
        )
      }
    }

  implicit def productDecoder[F[_], T, GenRepr <: HList, DefaultOptsRepr <: HList, S, I](
    implicit gen: LabelledGeneric.Aux[T, GenRepr],
    defaults: Default.AsOptions.Aux[T, DefaultOptsRepr],
    tag: TypeTag[T],
    F: Functor[F],
    decoder: Lazy[DerivedDecoderWithDefault[F, T, GenRepr, DefaultOptsRepr, S, I]],
    lp: LowPriority,
    refute: Refute[DecoderRefute[T, S, I]],
    refuteParser: Refute[Parser[T]],
    refuteMultiParser: Refute[MultiParser[F, T]]
  ): Decoder[F, S, T, I] =
    Decoder.make[F, S, T, I] { (path, settings, _, data) =>
      val newPath = path :+ PathElement.ClassName(tag.tpe.typeSymbol.name.toString)
      decoder.value.read(newPath, settings, defaults(), data).map(gen.from)
    }
}
