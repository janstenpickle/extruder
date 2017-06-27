package extruder.core

import cats.effect.IO
import cats.implicits._
import shapeless._
import shapeless.labelled.{field, FieldType}

import scala.reflect.runtime.universe.TypeTag

trait DerivedDecoders { self: Decoders with DecodeTypes =>
  implicit def cnilDecoder[F[_], E](implicit hints: Hint, AE: ExtruderApplicativeError[F, E]): Dec[F, CNil] =
    mkDecoder[F, CNil] { (path, _, _) =>
      IO.pure(
        AE.validationFailure(
          s"Could not find specified implementation of sealed type at configuration path '${hints.pathToStringWithType(path)}'"
        )
      )
    }

  implicit def cconsDecoder[F[_], E, K <: Symbol, H, T <: Coproduct](
    implicit key: Witness.Aux[K],
    headResolve: Dec[F, H],
    tailResolve: Lazy[Dec[F, T]],
    typeResolver: Lazy[Dec[F, Option[String]]],
    utils: Hint,
    AE: ExtruderApplicativeError[F, E],
    FM: IOFlatMap[F]
  ): Dec[F, FieldType[K, H] :+: T] =
    mkDecoder { (path, _, config) =>
      val onValidType: Option[String] => IO[F[FieldType[K, H] :+: T]] = {
        case None =>
          IO.pure(AE.missing(s"Could not type of sealed instance at path '${utils.pathToStringWithType(path)}'"))
        case Some(tpe) if tpe == key.value.name =>
          headResolve.read(path, None, config).map(AE.map(_)(v => Inl(field[K](v))))
        case _ => tailResolve.value.read(path, None, config).map(AE.map(_)(Inr(_)))
      }
      FM.flatMap(typeResolver.value.read(utils.pathWithType(path), None, config))(onValidType)
    }

  implicit def unionDecoder[F[_], E, T, V <: Coproduct](
    implicit gen: LabelledGeneric.Aux[T, V],
    underlying: Lazy[Dec[F, V]],
    AE: ExtruderApplicativeError[F, E],
    FM: IOFlatMap[F],
    lp: LowPriority
  ): Dec[F, T] =
    mkDecoder { (path, _, config) =>
      underlying.value.read(path, None, config).map(AE.map(_)(gen.from))
    }

  trait DerivedDecoderWithDefault[T, F[_], Repr <: HList, DefaultRepr <: HList] {
    def read(path: List[String], default: DefaultRepr, config: DecodeConfig): IO[F[Repr]]
  }

  implicit def hNilDerivedDecoder[T, F[_], E](
    implicit AE: ExtruderApplicativeError[F, E]
  ): DerivedDecoderWithDefault[T, F, HNil, HNil] =
    new DerivedDecoderWithDefault[T, F, HNil, HNil] {
      override def read(path: List[String], default: HNil, config: DecodeConfig): IO[F[HNil]] = IO.pure(AE.pure(HNil))
    }

  implicit def hConsDerivedDecoder[T, F[_], E, K <: Symbol, V, TailRepr <: HList, DefaultsTailRepr <: HList](
    implicit key: Witness.Aux[K],
    AE: ExtruderApplicativeError[F, E],
    decoder: Lazy[Dec[F, V]],
    tailDecoder: Lazy[DerivedDecoderWithDefault[T, F, TailRepr, DefaultsTailRepr]]
  ): DerivedDecoderWithDefault[T, F, FieldType[K, V] :: TailRepr, Option[V] :: DefaultsTailRepr] =
    new DerivedDecoderWithDefault[T, F, FieldType[K, V] :: TailRepr, Option[V] :: DefaultsTailRepr] {
      override def read(
        path: List[String],
        default: Option[V] :: DefaultsTailRepr,
        config: DecodeConfig
      ): IO[F[::[FieldType[K, V], TailRepr]]] = {
        val fieldName = key.value.name

        for {
          head <- decoder.value.read(path :+ fieldName, default.head, config)
          tail <- tailDecoder.value.read(path, default.tail, config)
        } yield (head |@| tail).map((h, t) => field[K](h) :: t)
      }
    }

  implicit def productDecoder[F[_], E, T, GenRepr <: HList, DefaultOptsRepr <: HList](
    implicit gen: LabelledGeneric.Aux[T, GenRepr],
    defaults: Default.AsOptions.Aux[T, DefaultOptsRepr],
    tag: TypeTag[T],
    AE: ExtruderApplicativeError[F, E],
    decoder: Lazy[DerivedDecoderWithDefault[T, F, GenRepr, DefaultOptsRepr]]
  ): Dec[F, T] = {
    lazy val className: String = tag.tpe.typeSymbol.name.toString
    mkDecoder { (path, _, config) =>
      decoder.value.read(path :+ className, defaults(), config).map(AE.map(_)(gen.from))
    }
  }

}
