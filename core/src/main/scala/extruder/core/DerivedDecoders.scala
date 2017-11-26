package extruder.core

import cats.implicits._
import extruder.effect.ExtruderAsync
import shapeless._
import shapeless.labelled.{field, FieldType}

import scala.reflect.runtime.universe.TypeTag

trait DerivedDecoders { self: Decoders with DecodeTypes =>
  implicit def cnilDecoder[F[_]](implicit hints: Hint, F: ExtruderAsync[F]): Dec[F, CNil] =
    mkDecoder[F, CNil] { (path, _, _) =>
      F.validationFailure(
        s"Could not find specified implementation of sealed type at path '${hints.pathToStringWithType(path)}'"
      )
    }

  implicit def cconsDecoder[F[_], K <: Symbol, H, T <: Coproduct](
    implicit key: Witness.Aux[K],
    headResolve: Dec[F, H],
    tailResolve: Lazy[Dec[F, T]],
    typeResolver: Lazy[Dec[F, Option[String]]],
    hints: Hint,
    F: ExtruderAsync[F]
  ): Dec[F, FieldType[K, H] :+: T] =
    mkDecoder { (path, _, data) =>
      val onValidType: Option[String] => F[FieldType[K, H] :+: T] = {
        case None =>
          F.missing(s"Could not type of sealed instance at path '${hints.pathToStringWithType(path)}'")
        case Some(tpe) if tpe == key.value.name =>
          headResolve.read(path, None, data).map(v => Inl(field[K](v)))
        case _ => tailResolve.value.read(path, None, data).map(Inr(_))
      }
      F.flatMap(typeResolver.value.read(hints.pathWithType(path), None, data))(onValidType)
    }

  implicit def unionDecoder[F[_], T, V <: Coproduct](
    implicit gen: LabelledGeneric.Aux[T, V],
    underlying: Lazy[Dec[F, V]],
    F: ExtruderAsync[F],
    lp: LowPriority
  ): Dec[F, T] =
    mkDecoder { (path, _, data) =>
      underlying.value.read(path, None, data).map(gen.from)
    }

  trait DerivedDecoderWithDefault[T, F[_], Repr <: HList, DefaultRepr <: HList] {
    def read(path: List[String], default: DefaultRepr, data: DecodeData): F[Repr]
  }

  implicit def hNilDerivedDecoder[T, F[_]](implicit F: ExtruderAsync[F]): DerivedDecoderWithDefault[T, F, HNil, HNil] =
    new DerivedDecoderWithDefault[T, F, HNil, HNil] {
      override def read(path: List[String], default: HNil, data: DecodeData): F[HNil] = F.pure(HNil)
    }

  implicit def hConsDerivedDecoder[T, F[_], K <: Symbol, V, TailRepr <: HList, DefaultsTailRepr <: HList](
    implicit key: Witness.Aux[K],
    F: ExtruderAsync[F],
    decoder: Lazy[Dec[F, V]],
    tailDecoder: Lazy[DerivedDecoderWithDefault[T, F, TailRepr, DefaultsTailRepr]]
  ): DerivedDecoderWithDefault[T, F, FieldType[K, V] :: TailRepr, Option[V] :: DefaultsTailRepr] =
    new DerivedDecoderWithDefault[T, F, FieldType[K, V] :: TailRepr, Option[V] :: DefaultsTailRepr] {
      override def read(
        path: List[String],
        default: Option[V] :: DefaultsTailRepr,
        data: DecodeData
      ): F[::[FieldType[K, V], TailRepr]] = {
        val fieldName = key.value.name

        F.ap2(F.pure((h: V, t: TailRepr) => field[K](h) :: t))(
          decoder.value.read(path :+ fieldName, default.head, data),
          tailDecoder.value.read(path, default.tail, data)
        )
      }
    }

  implicit def productDecoder[F[_], T, GenRepr <: HList, DefaultOptsRepr <: HList](
    implicit gen: LabelledGeneric.Aux[T, GenRepr],
    defaults: Default.AsOptions.Aux[T, DefaultOptsRepr],
    tag: TypeTag[T],
    F: ExtruderAsync[F],
    decoder: Lazy[DerivedDecoderWithDefault[T, F, GenRepr, DefaultOptsRepr]]
  ): Dec[F, T] = {
    lazy val className: String = tag.tpe.typeSymbol.name.toString
    mkDecoder { (path, _, data) =>
      decoder.value.read(path :+ className, defaults(), data).map(gen.from)
    }
  }

}
