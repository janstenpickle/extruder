package extruder.core

import cats.syntax.cartesian._
import cats.syntax.validated._
import shapeless._
import shapeless.ops.hlist.{ConstMapper, Mapper, RightFolder, Zip}
import shapeless.ops.record._
import shapeless.syntax.std.tuple._
import extruder.syntax.validation.ConfigValidation

import scala.reflect.runtime.universe.TypeTag

case class ConfigConstructor[T <: Product with Serializable](prefix: Option[Seq[String]] = None)
                                                            (implicit tag: TypeTag[T]) {
  import ConfigConstructor._

  val className: String = tag.tpe.typeSymbol.name.toString
  val realPrefix: Seq[String] = prefix.fold(Seq(className))(_ :+ className)

  def resolve[GenRepr <: HList,
              DefaultOptsRepr <: HList,
              LGenRepr <: HList,
              KeysRepr <: HList,
              ConstRepr <: HList,
              ZipperRepr <: HList,
              PrefixZipperRepr <: HList,
              MapperRepr <: HList](implicit gen: Generic[T],
                                   genAux: Generic.Aux[T, GenRepr],
                                   defaultOpts: Default.AsOptions[T],
                                   defaultOptsAux: Default.AsOptions.Aux[T, DefaultOptsRepr],
                                   lGenAux: LabelledGeneric.Aux[T, LGenRepr],
                                   keys: Keys[LGenRepr],
                                   keysAux: Keys.Aux[LGenRepr, KeysRepr],
                                   constMapper: ConstMapper[Seq[String], KeysRepr],
                                   constMapperAux: ConstMapper.Aux[Seq[String], KeysRepr, ConstRepr],
                                   prefixZipper: Zip[KeysRepr :: ConstRepr :: HNil],
                                   prefixZipperAux: Zip.Aux[KeysRepr :: ConstRepr :: HNil, PrefixZipperRepr],
                                   zipper: Zip[PrefixZipperRepr :: DefaultOptsRepr :: HNil],
                                   zipperAux: Zip.Aux[PrefixZipperRepr :: DefaultOptsRepr :: HNil, ZipperRepr],
                                   mapper: Mapper[readConfig.type, ZipperRepr],
                                   mapperAux: Mapper.Aux[readConfig.type, ZipperRepr, MapperRepr],
                                   rightFolder: RightFolder[MapperRepr, ConfigValidation[HNil], folder.type],
                                   rightFolderAux: RightFolder.Aux[MapperRepr, ConfigValidation[HNil], folder.type, ConfigValidation[GenRepr]]): ConfigValidation[T] = {
    val keys = Keys[LGenRepr].apply()
    keys.zip(keys.mapConst(realPrefix)).
      zip(Default.AsOptions[T].apply()).
      map(readConfig).
      foldRight((HNil :: HNil).tail.validNel[ValidationFailure])(folder).
      map(Generic[T].from)
  }
}

object ConfigConstructor {
  object readConfig extends Poly1 {
    implicit def caseAny[A <: Symbol, B](implicit resolver: Resolver[B]): Case.Aux[((A, Seq[String]), Option[B]), ConfigValidation[B]] =
      at[((A, Seq[String]), Option[B])]{ case ((key, prefix), default) => resolver.read(prefix :+ key.name, default) }
  }

  object folder extends Poly2 {
    implicit def caseHList[A, B <: HList]: Case.Aux[ConfigValidation[A], ConfigValidation[B], ConfigValidation[A :: B]] =
      at[ConfigValidation[A], ConfigValidation[B]]((a, b) => (a |@| b).map((aa, bb) => aa :: bb))
  }
}