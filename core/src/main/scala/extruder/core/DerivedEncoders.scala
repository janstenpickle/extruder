package extruder.core

import cats.syntax.cartesian._
import cats.syntax.validated._
import shapeless._
import shapeless.labelled.FieldType
import shapeless.ops.hlist.{ConstMapper, Mapper, RightFolder, Zip}
import shapeless.ops.record.Keys

import scala.reflect.runtime.universe.TypeTag

trait DerivedEncoders[C, E[T] <: Encoder[T, C]] { self: Encoders[C, E] with UtilsMixin =>
  implicit val cnilEncoder: E[CNil] = mkEncoder((_, _) =>
    ValidationFailure(s"Impossible!").invalidNel
  )

  implicit def cconsEncoder[K <: Symbol, H, T <: Coproduct](implicit key: Witness.Aux[K],
                                                     headEncode: Lazy[E[H]],
                                                     tailEncode: Lazy[E[T]],
                                                     typeEncode: Lazy[E[String]]): E[FieldType[K, H] :+: T] =
  mkEncoder((path, value) =>
    (typeEncode.value.write(utils.pathWithType(path), key.value.name) |@| (value match {
      case Inl(h) => headEncode.value.write(path, h)
      case Inr(t) => tailEncode.value.write(path, t)
    })).map(monoid.combine)
  )

  implicit def unionEncoder[T, O <: Coproduct](implicit gen: LabelledGeneric.Aux[T, O],
                                   underlying: Lazy[E[O]]): E[T] =
    mkEncoder((path, value) => underlying.value.write(path, gen.to(value)))

  // scalastyle:off
  implicit def productEncoder[T,
                             GenRepr <: HList,
                             LGenRepr <: HList,
                             KeysRepr <: HList,
                             ConstRepr <: HList,
                             ZipperRepr <: HList,
                             PrefixZipperRepr <: HList,
                             MapperRepr <: HList](implicit gen: Generic.Aux[T, GenRepr],
                                                  lGen: LabelledGeneric.Aux[T, LGenRepr],
                                                  keys: Keys.Aux[LGenRepr, KeysRepr],
                                                  constMapper: ConstMapper.Aux[Seq[String], KeysRepr, ConstRepr],
                                                  prefixZipper: Zip.Aux[KeysRepr :: ConstRepr :: HNil, PrefixZipperRepr],
                                                  zipper: Zip.Aux[PrefixZipperRepr :: GenRepr :: HNil, ZipperRepr],
                                                  lazyMapper: Lazy[Mapper.Aux[writeConfig.type, ZipperRepr, MapperRepr]],
                                                  rightFolder: RightFolder.Aux[MapperRepr, ConfigValidation[C], folder.type, ConfigValidation[C]],
                                                  tag: TypeTag[T]): E[T] = {
    implicit val mapper: Mapper.Aux[writeConfig.type, ZipperRepr, MapperRepr] = lazyMapper.value
    lazy val className: String = tag.tpe.typeSymbol.name.toString
    val keyNames = Keys[LGenRepr].apply()
    mkEncoder((path, value) =>
      keyNames
        .zip(keyNames.mapConst(path :+ className))
        .zip(gen.to(value))
        .map(writeConfig)
        .foldRight(monoid.empty.validNel[ValidationError])(folder)
    )
  }
  // scalastyle:on

  object writeConfig extends Poly1 {
    implicit def caseAny[A <: Symbol, B](implicit encoder: Lazy[E[B]]): Case.Aux[((A, Seq[String]), B), ConfigValidation[C]] =
      at[((A, Seq[String]), B)]{ case ((key, prefix), value) => encoder.value.write(prefix :+ key.name, value) }
  }

  // scalastyle:off
  object folder extends Poly2 {
    implicit def caseHList: Case.Aux[ConfigValidation[C], ConfigValidation[C], ConfigValidation[C]] =
      at[ConfigValidation[C], ConfigValidation[C]]((a, b) => (a |@| b).map(monoid.combine))
  }
  // scalastyle:on


  implicit def caseObjectEncoder[T](implicit gen: Generic.Aux[T, HNil],
                                    stringEncoder: Lazy[E[String]]): E[T] =
    mkEncoder((path, value) => stringEncoder.value.write(utils.pathWithType(path), value.toString))
}
