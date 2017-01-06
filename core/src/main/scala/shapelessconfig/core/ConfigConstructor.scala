package shapelessconfig.core

import cats.data.Validated.{Invalid, Valid}
import cats.syntax.cartesian._
import cats.syntax.either._
import cats.syntax.validated._
import mouse.string._
import shapeless._
import shapeless.ops.hlist.{ConstMapper, Mapper, RightFolder, Zip}
import shapeless.ops.record._
import shapeless.syntax.std.tuple._
import shapelessconfig.core.validation.ConfigValidation

import scala.reflect.runtime.universe.TypeTag

trait Resolver[T] {
  def read(path: Seq[String], default: Option[T]): ConfigValidation[T]
}

object Resolver {
  def apply[T](f: (Seq[String], Option[T]) => ConfigValidation[T]) = new Resolver[T] {
    override def read(path: Seq[String],
                      default: Option[T]): ConfigValidation[T] = f(path, default)
  }
}

trait Resolvers extends Serializable {
  def resolveConfig(path: Seq[String]): ConfigValidation[Option[String]]

  def pathToString(path: Seq[String]): String

  def errorMsg[T](path: Seq[String]): ConfigValidation[T] =
    s"Could not find System Property at '${pathToString(path)}' and no default available".invalidNel[T]

  def resolveEither[T](parser: String => Either[Throwable, T])
                      (path: Seq[String], default: Option[T]): ConfigValidation[T] =
    resolve(parser.andThen(
      _.leftMap(ex => s"Could not parse value for '${pathToString(path)}': ${ex.getMessage}").toValidatedNel
    ))(path, default)

  def resolve[T](parser: String => ConfigValidation[T])(path: Seq[String], default: Option[T]): ConfigValidation[T] =
    (resolveConfig(path), default) match {
      case (Valid(None), None) => errorMsg[T](path)
      case (Valid(None), Some(v)) => v.validNel[String]
      case (Valid(Some(str)), _) => parser(str)
      case (err @ Invalid(_), _) => err
    }

  implicit val string: Resolver[String] = Resolver(resolve(_.validNel))
  implicit val int: Resolver[Int] = Resolver(resolveEither(_.parseInt))
  implicit val long: Resolver[Long] = Resolver(resolveEither(_.parseLong))
  implicit val double: Resolver[Double] = Resolver(resolveEither(_.parseDouble))
  implicit val float: Resolver[Float] = Resolver(resolveEither(_.parseFloat))
  implicit val short: Resolver[Short] = Resolver(resolveEither(_.parseShort))
  implicit val byte: Resolver[Byte] = Resolver(resolveEither(_.parseByte))
  implicit val boolean: Resolver[Boolean] = Resolver(resolveEither(_.parseBoolean))

  implicit def option[T](implicit resolver: Resolver[T]): Resolver[Option[T]] =
    Resolver((path, default) =>
      (resolveConfig(path), default) match {
        case (Valid(None), None) => None.validNel
        case (Valid(None), Some(d)) => d.validNel
        case (Valid(Some(_)), _) => resolver.read(path, None).map(Some(_))
        case (err @ Invalid(_), _) => err
      }
    )
}

case class ConfigConstructor[T <: Product with Serializable](prefix: Option[Seq[String]] = None)
                                                            (implicit tag: TypeTag[T]) {
  import ConfigConstructor._

  val realPrefix: Seq[String] = prefix.getOrElse(Seq(tag.tpe.typeSymbol.name.toString.toLowerCase))

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
      foldRight((HNil :: HNil).tail.validNel[String])(folder).
      map(Generic[T].from)
  }
}

object ConfigConstructor {
  object readConfig extends Poly1 {
    implicit def caseAny[A <: Symbol, B](implicit resolver: Resolver[B]): Case.Aux[((A, Seq[String]), Option[B]), ConfigValidation[B]] =
      at[((A, Seq[String]), Option[B])]{ case ((key, prefix), default) => resolver.read(prefix :+ key.name, default)}
  }

  object folder extends Poly2 {
    implicit def caseHList[A, B <: HList]: Case.Aux[ConfigValidation[A], ConfigValidation[B], ConfigValidation[A :: B]] =
      at[ConfigValidation[A], ConfigValidation[B]]((a, b) => (a |@| b).map((aa, bb) => aa :: bb))
  }
}