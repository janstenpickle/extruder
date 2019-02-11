package extruder.core

import cats.{Applicative, Functor, Monad}
import cats.implicits._
import shapeless._
import shapeless.labelled.{field, FieldType}

import scala.reflect.runtime.universe.TypeTag

trait GenericDecoderTInstances {
  implicit def cnilDecoder[F[_]: Functor, S <: Settings, I](
    implicit error: ExtruderErrors[F]
  ): DecoderT[F, S, CNil, I] =
    DecoderT.make[F, S, CNil, I] { (path, settings, _, _) =>
      error.validationFailure(
        s"Could not find specified implementation of sealed type at path '${settings.pathToStringWithType(path)}'"
      )
    }

  implicit def cconsDecoder[F[_], K <: Symbol, H, T <: Coproduct, S <: Settings, I](
    implicit key: Witness.Aux[K],
    headResolve: DecoderT[F, S, H, I],
    tailResolve: Lazy[DecoderT[F, S, T, I]],
    typeResolver: Lazy[DecoderT[F, S, Option[String], I]],
    F: Monad[F],
    errors: ExtruderErrors[F]
  ): DecoderT[F, S, FieldType[K, H] :+: T, I] =
    DecoderT.make[F, S, FieldType[K, H] :+: T, I] { (path, settings, _, data) =>
      def head: F[FieldType[K, H] :+: T] = headResolve.read(path, settings, None, data).map(v => Inl(field[K](v)))
      def tail: F[FieldType[K, H] :+: T] = tailResolve.value.read(path, settings, None, data).map(Inr(_))
      val onValidType: Option[String] => F[FieldType[K, H] :+: T] = {
        case None => errors.fallback(head)(tail)
        case Some(tpe) if tpe == key.value.name => head
        case _ => tail
      }
      F.flatMap(typeResolver.value.read(settings.pathWithType(path), settings, None, data))(onValidType)
    }

  implicit def unionDecoder[F[_], T, V <: Coproduct, S, I](
    implicit gen: LabelledGeneric.Aux[T, V],
    underlying: Lazy[DecoderT[F, S, V, I]],
    F: Functor[F],
    lp: LowPriority,
    refute: Refute[DecoderTRefute[T, S, I]],
    refuteParser: Refute[Parser[T]],
    refuteMultiParser: Refute[MultiParser[F, T]],
    neOpt: T <:!< Option[_],
    neCol: T <:!< TraversableOnce[_]
  ): DecoderT[F, S, T, I] =
    DecoderT.make[F, S, T, I] { (path, settings, _, data) =>
      underlying.value.read(path, settings, None, data).map(gen.from)
    }

  trait DerivedDecoderWithDefault[F[_], T, Repr <: HList, DefaultRepr <: HList, S, I] {
    def read(path: List[String], settings: S, default: DefaultRepr, data: I): F[Repr]
  }

  implicit def hNilDerivedDecoder[F[_], T, S, D](
    implicit F: Applicative[F]
  ): DerivedDecoderWithDefault[F, T, HNil, HNil, S, D] =
    new DerivedDecoderWithDefault[F, T, HNil, HNil, S, D] {
      override def read(path: List[String], settings: S, default: HNil, data: D): F[HNil] = F.pure(HNil)
    }

  implicit def hConsDerivedDecoder[T, F[_], K <: Symbol, V, TailRepr <: HList, DefaultsTailRepr <: HList, S, I](
    implicit key: Witness.Aux[K],
    F: Applicative[F],
    decoder: Lazy[DecoderT[F, S, V, I]],
    tailDecoder: Lazy[DerivedDecoderWithDefault[F, T, TailRepr, DefaultsTailRepr, S, I]]
  ): DerivedDecoderWithDefault[F, T, FieldType[K, V] :: TailRepr, Option[V] :: DefaultsTailRepr, S, I] =
    new DerivedDecoderWithDefault[F, T, FieldType[K, V] :: TailRepr, Option[V] :: DefaultsTailRepr, S, I] {
      override def read(
        path: List[String],
        settings: S,
        default: Option[V] :: DefaultsTailRepr,
        data: I
      ): F[::[FieldType[K, V], TailRepr]] = {
        val fieldName = key.value.name

        F.ap2(F.pure((h: V, t: TailRepr) => field[K](h) :: t))(
          decoder.value.read(path :+ fieldName, settings, default.head, data),
          tailDecoder.value.read(path, settings, default.tail, data)
        )
      }
    }

  implicit def productDecoder[F[_], T, GenRepr <: HList, DefaultOptsRepr <: HList, S <: Settings, I](
    implicit gen: LabelledGeneric.Aux[T, GenRepr],
    defaults: Default.AsOptions.Aux[T, DefaultOptsRepr],
    tag: TypeTag[T],
    F: Functor[F],
    decoder: Lazy[DerivedDecoderWithDefault[F, T, GenRepr, DefaultOptsRepr, S, I]],
    lp: LowPriority,
    refute: Refute[DecoderTRefute[T, S, I]],
    refuteParser: Refute[Parser[T]],
    refuteMultiParser: Refute[MultiParser[F, T]]
  ): DecoderT[F, S, T, I] =
    DecoderT.make[F, S, T, I] { (path, settings, _, data) =>
      val newPath =
        if (settings.includeClassNameInPath) path :+ tag.tpe.typeSymbol.name.toString
        else path
      decoder.value.read(newPath, settings, defaults(), data).map(gen.from)
    }
}
