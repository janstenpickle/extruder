package org.janstenpickle.configconstructor

import cats.data.ValidatedNel
import cats.syntax.cartesian._
import cats.syntax.validated._
import org.janstenpickle.configconstructor.ConfigConstructor.ConfigValidation
import shapeless._
import shapeless.ops.hlist.{ConstMapper, Mapper, RightFolder, Zip}
import shapeless.ops.record._
import shapeless.syntax.std.tuple._

import scala.reflect.runtime.universe.TypeTag

case class CC(a: String = "test", b: String = "test2", c: Int = 100, d: CC2)

case class CC2(x: String = "test4", y: Option[Int] = Some(232))


trait Resolver[T] {
  def read(path: Seq[String], default: Option[T]): ConfigValidation[T]
}

object Resolver {
  def apply[T](f: (Seq[String], Option[T]) => ConfigValidation[T]) = new Resolver[T] {
    override def read(path: Seq[String],
                      default: Option[T]): ConfigValidation[T] = f(path, default)
  }
}

class ConfigConstructor[T <: Product with Serializable](implicit tag: TypeTag[T]) {
  import ConfigConstructor._

  val prefix: Seq[String] = List(tag.tpe.typeSymbol.name.toString.toLowerCase)

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
    keys.zip(keys.mapConst(prefix)).
      zip(Default.AsOptions[T].apply()).
      map(readConfig).
      foldRight((HNil :: HNil).tail.validNel[String])(folder).
      map(Generic[T].from)
  }

}

object ConfigConstructor {
  type ConfigValidation[T] = ValidatedNel[String, T]

  object readConfig extends Poly1 {
    implicit def caseAny[A <: Symbol, B](implicit resolver: Resolver[B]): Case.Aux[((A, Seq[String]), Option[B]), ConfigValidation[B]] =
      at[((A, Seq[String]), Option[B])]{ case ((key, prefix), default) => resolver.read(prefix :+ key.name, default)}
  }

  object folder extends Poly2 {
    implicit def caseHList[A, B <: HList]: Case.Aux[ConfigValidation[A], ConfigValidation[B], ConfigValidation[A :: B]] =
      at[ConfigValidation[A], ConfigValidation[B]]((a, b) => (a |@| b).map((aa, bb) => aa :: bb))
  }

  def apply[T <: Product with Serializable : TypeTag]: ConfigConstructor[T] = new ConfigConstructor[T]()
}


object Test extends App {
  implicit val stringResolver: Resolver[String] =
    Resolver[String]((_, default) => default.fold("Empty".invalidNel[String])(_.validNel))

  implicit val intResolver: Resolver[Int] =
    Resolver[Int]((_, default) => default.fold("Empty".invalidNel[Int])(_.validNel))


  implicit def optResolver[T](implicit resolver: Resolver[T]): Resolver[Option[T]] =
    (path: Seq[String], default: Option[Option[T]]) =>
      default.fold[ConfigValidation[Option[T]]]("".invalidNel[Option[T]])(
        _.fold[ConfigValidation[Option[T]]](None.validNel[String])(x => resolver.read(path, Some(x)).map(Some(_))))

  implicit val cc2Resolver: Resolver[CC2] = (_: Seq[String], _: Option[CC2]) => ConfigConstructor.apply[CC2].resolve

  println(ConfigConstructor.apply[CC].resolve)
}



