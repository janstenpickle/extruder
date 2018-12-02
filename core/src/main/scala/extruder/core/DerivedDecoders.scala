package extruder.core

import cats.implicits._
import shapeless._
import shapeless.labelled.{field, FieldType}

import scala.reflect.runtime.universe.TypeTag

trait DerivedDecoders { self: Decoders with DecodeTypes =>
  implicit def cnilDecoder[F[_]](implicit F: DecEff[F], error: ExtruderErrors[F]): DecT[F, CNil] =
    mkDecoder[F, CNil] { (path, settings, _, _) =>
      error.validationFailure(
        s"Could not find specified implementation of sealed type at path '${settings.pathToStringWithType(path)}'"
      )
    }

  implicit def cconsDecoder[F[_], K <: Symbol, H, T <: Coproduct](
    implicit key: Witness.Aux[K],
    headResolve: DecT[F, H],
    tailResolve: Lazy[DecT[F, T]],
    typeResolver: Lazy[DecT[F, Option[String]]],
    F: DecEff[F],
    errors: ExtruderErrors[F]
  ): DecT[F, FieldType[K, H] :+: T] =
    mkDecoder { (path, settings, _, data) =>
      def head: F[FieldType[K, H] :+: T] = headResolve.read(path, settings, None, data).map(v => Inl(field[K](v)))
      def tail: F[FieldType[K, H] :+: T] = tailResolve.value.read(path, settings, None, data).map(Inr(_))
      val onValidType: Option[String] => F[FieldType[K, H] :+: T] = {
        case None => errors.fallback(head)(tail)
        case Some(tpe) if tpe == key.value.name => head
        case _ => tail
      }
      F.flatMap(typeResolver.value.read(settings.pathWithType(path), settings, None, data))(onValidType)
    }

  implicit def unionDecoder[F[_], T, V <: Coproduct](
    implicit gen: LabelledGeneric.Aux[T, V],
    underlying: Lazy[DecT[F, V]],
    F: DecEff[F],
    lp: LowPriority,
    refute: Refute[DecoderRefute[T]],
    refuteParser: Refute[Parser[T]],
    refuteMultiParser: Refute[MultiParser[F, T]],
    neOpt: T <:!< Option[_],
    neCol: T <:!< TraversableOnce[_]
  ): DecT[F, T] =
    mkDecoder { (path, settings, _, data) =>
      underlying.value.read(path, settings, None, data).map(gen.from)
    }

  trait DerivedDecoderWithDefault[T, F[_], Repr <: HList, DefaultRepr <: HList] {
    def read(path: List[String], settings: Sett, default: DefaultRepr, data: DecodeData): F[Repr]
  }

  implicit def hNilDerivedDecoder[T, F[_]](implicit F: DecEff[F]): DerivedDecoderWithDefault[T, F, HNil, HNil] =
    new DerivedDecoderWithDefault[T, F, HNil, HNil] {
      override def read(path: List[String], settings: Sett, default: HNil, data: DecodeData): F[HNil] = F.pure(HNil)
    }

  implicit def hConsDerivedDecoder[T, F[_], K <: Symbol, V, TailRepr <: HList, DefaultsTailRepr <: HList](
    implicit key: Witness.Aux[K],
    F: DecEff[F],
    decoder: Lazy[DecT[F, V]],
    tailDecoder: Lazy[DerivedDecoderWithDefault[T, F, TailRepr, DefaultsTailRepr]]
  ): DerivedDecoderWithDefault[T, F, FieldType[K, V] :: TailRepr, Option[V] :: DefaultsTailRepr] =
    new DerivedDecoderWithDefault[T, F, FieldType[K, V] :: TailRepr, Option[V] :: DefaultsTailRepr] {
      override def read(
        path: List[String],
        settings: Sett,
        default: Option[V] :: DefaultsTailRepr,
        data: DecodeData
      ): F[::[FieldType[K, V], TailRepr]] = {
        val fieldName = key.value.name

        F.ap2(F.pure((h: V, t: TailRepr) => field[K](h) :: t))(
          decoder.value.read(path :+ fieldName, settings, default.head, data),
          tailDecoder.value.read(path, settings, default.tail, data)
        )
      }
    }

  implicit def productDecoder[F[_], T, GenRepr <: HList, DefaultOptsRepr <: HList](
    implicit gen: LabelledGeneric.Aux[T, GenRepr],
    defaults: Default.AsOptions.Aux[T, DefaultOptsRepr],
    tag: TypeTag[T],
    F: DecEff[F],
    decoder: Lazy[DerivedDecoderWithDefault[T, F, GenRepr, DefaultOptsRepr]],
    lp: LowPriority,
    refute: Refute[DecRefute[T]],
    refuteParser: Refute[Parser[T]],
    refuteMultiParser: Refute[MultiParser[F, T]]
  ): DecT[F, T] =
    mkDecoder { (path, settings, _, data) =>
      val newPath =
        if (settings.includeClassNameInPath) path :+ tag.tpe.typeSymbol.name.toString
        else path
      decoder.value.read(newPath, settings, defaults(), data).map(gen.from)
    }

}
