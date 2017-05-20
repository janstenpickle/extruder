package extruder.core

import cats.data.Validated.{Invalid, Valid}
import cats.implicits._
import shapeless.labelled.{FieldType, field}
import shapeless.ops.hlist.{ConstMapper, Mapper, RightFolder, Zip}
import shapeless.ops.record._
import shapeless.syntax.std.tuple._
import shapeless._

import scala.reflect.runtime.universe.TypeTag

trait DerivedDecoders[C, D[T] <: Decoder[T, C]] extends ResolutionCommon { self: Decoders[C, D] =>
  import DerivedDecoders._

  implicit val cnilDecoder: D[CNil] = mkDecoder((path, _, _) => ValidationFailure(
    s"Could not find specified implementation of sealed type at configuration path '${pathToStringWithType(path)}'"
  ).invalidNel)

  implicit def cconsDecoder[K <: Symbol, H, T <: Coproduct](implicit key: Witness.Aux[K],
                                                            headResolve: Lazy[D[H]],
                                                            tailResolve: Lazy[D[T]],
                                                            typeResolver: Lazy[D[Option[String]]]): D[FieldType[K, H] :+: T] =
      mkDecoder((path, _, config) =>
      typeResolver.value.read(pathWithType(path), None, config) match {
        case Valid(None) => Missing(
          s"Could not type of sealed instance at path '${pathToStringWithType(path)}'"
        ).invalidNel
        case Valid(Some(tpe)) if tpe == key.value.name =>
          headResolve.value.read(path, None, config).map(v => Inl(field[K](v)))
        case Valid(_) => tailResolve.value.read(path, None, config).map(Inr(_))
        case err @ Invalid(_) => err
      }
    )

  implicit def unionDecoder[T, V <: Coproduct](implicit gen: LabelledGeneric.Aux[T, V],
                                               underlying: Lazy[D[V]]): D[T] =
    mkDecoder((path, _, config) => underlying.value.read(path, None, config).map(gen.from))

  // scalastyle:off
//  implicit def productDecoder[T,
//                              GenRepr <: HList,
//                              DefaultOptsRepr <: HList,
//                              LGenRepr <: HList,
//                              KeysRepr <: HList,
//                              ConstRepr <: HList,
//                              ZipperRepr <: HList,
//                              PrefixZipperRepr <: HList,
//                              MapperRepr <: HList](implicit gen: Generic.Aux[T, GenRepr],
//                                                   defaultOpts: Default.AsOptions.Aux[T, DefaultOptsRepr],
//                                                   lGen: LabelledGeneric.Aux[T, LGenRepr],
//                                                   keys: Keys.Aux[LGenRepr, KeysRepr],
//                                                   constMapper: ConstMapper.Aux[(Seq[String], C), KeysRepr, ConstRepr],
//                                                   prefixZipper: Zip.Aux[KeysRepr :: ConstRepr :: HNil, PrefixZipperRepr],
//                                                   zipper: Zip.Aux[PrefixZipperRepr :: DefaultOptsRepr :: HNil, ZipperRepr],
//                                                   lazyMapper: Lazy[Mapper.Aux[readConfig.type, ZipperRepr, MapperRepr]],
//                                                   rightFolder: RightFolder.Aux[MapperRepr, ConfigValidation[HNil], folder.type, ConfigValidation[GenRepr]],
//                                                   tag: TypeTag[T]): D[T] = {
//    lazy val className: String = tag.tpe.typeSymbol.name.toString
//    val keyNames = Keys[LGenRepr].apply()
//    mkDecoder((path, _, config) =>
//      keyNames.zip(keyNames.mapConst((path :+ className, config))).
//        zip(Default.AsOptions[T].apply()).
//        map(readConfig)(lazyMapper.value).
//        foldRight((HNil :: HNil).tail.validNel[ValidationError])(folder).
//        map(Generic[T].from)
//    )
//  }

  implicit def anyDecoder[T, GenRep <: HList, ParamRepr <: HList](implicit gen: Default.AsOptions.Aux[T, GenRep], mapper: Lazy[Mapper[cunt.type, GenRep]], parameters: NewParameters.Aux[T, ParamRepr]): D[T] = {
    println(Default.AsOptions[T].apply().map(cunt)(mapper.value))
    mkDecoder((path, _, config) => ValidationFailure(parameters.eval(path).toString).invalidNel)
  }

  object cunt extends Poly1 {
    implicit def caseAny[A, ParamRepr](implicit params: Lazy[NewParameters.Aux[A, ParamRepr]]): Case.Aux[Option[A], ParamRepr] = at[Option[A]](_ => params.value.eval(Seq.empty))
  }

  // scalastyle:on

  implicit def objectDecoder[T](implicit gen: Generic.Aux[T, HNil]): D[T] =
    mkDecoder((_, _, _) => gen.from((HNil :: HNil).tail).validNel)

  object readConfig extends Poly1 {
    implicit def caseAny[A <: Symbol, B](implicit decoder: Lazy[D[B]]): Case.Aux[((A, (Seq[String], C)), Option[B]), ConfigValidation[B]] =
      at[((A, (Seq[String], C)), Option[B])]{ case ((key, (prefix, config)), default) => decoder.value.read(prefix :+ key.name, default, config) }
  }
}

object DerivedDecoders {
  // scalastyle:off
  object folder extends Poly2 {
    implicit def caseHList[A, B <: HList]: Case.Aux[ConfigValidation[A], ConfigValidation[B], ConfigValidation[A :: B]] =
      at[ConfigValidation[A], ConfigValidation[B]]((a, b) => (a |@| b).map(_ :: _))
  }
  // scalastyle:on
}
