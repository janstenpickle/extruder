package extruder.core

import cats.data.Validated.{Invalid, Valid}
import cats.syntax.cartesian._
import cats.syntax.validated._
import shapeless._
import shapeless.ops.hlist.{ConstMapper, Mapper, RightFolder, Zip}
import shapeless.ops.record._
import shapeless.syntax.std.tuple._

import scala.reflect.runtime.universe.TypeTag

case class Extruder[T](resolvers: ResolversBase)
                      (implicit tag: TypeTag[T]) {
  import Extruder._

  lazy val className: String = tag.tpe.typeSymbol.name.toString

  def productResolver[GenRepr <: HList,
                      DefaultOptsRepr <: HList,
                      LGenRepr <: HList,
                      KeysRepr <: HList,
                      ConstRepr <: HList,
                      ZipperRepr <: HList,
                      PrefixZipperRepr <: HList,
                      MapperRepr <: HList](implicit gen: Generic.Aux[T, GenRepr],
                                           defaultOpts: Default.AsOptions.Aux[T, DefaultOptsRepr],
                                           lGen: LabelledGeneric.Aux[T, LGenRepr],
                                           keys: Keys.Aux[LGenRepr, KeysRepr],
                                           constMapper: ConstMapper.Aux[Seq[String], KeysRepr, ConstRepr],
                                           prefixZipper: Zip.Aux[KeysRepr :: ConstRepr :: HNil, PrefixZipperRepr],
                                           zipper: Zip.Aux[PrefixZipperRepr :: DefaultOptsRepr :: HNil, ZipperRepr],
                                           mapper: Mapper.Aux[readConfig.type, ZipperRepr, MapperRepr],
                                           rightFolder: RightFolder.Aux[MapperRepr, ConfigValidation[HNil], folder.type,
                                           ConfigValidation[GenRepr]]): Resolver[T] = {
    val keyNames = Keys[LGenRepr].apply()
    Resolver((path, default) =>
      keyNames.zip(keyNames.mapConst(path :+ className)).
        zip(Default.AsOptions[T].apply()).
        map(readConfig).
        foldRight((HNil :: HNil).tail.validNel[ValidationFailure])(folder).
        map(Generic[T].from)
    )
  }

  def unionResolver[C <: Coproduct](implicit gen: LabelledGeneric.Aux[T, C],
                                    underlying: Resolver[Option[C]]): Resolver[T] =
    Resolver((path, default) =>
      (underlying.read(path, None), default) match {
        case (Valid(None), None) => ValidationFailure(
          s"Could not resolve instance of '$className' at path '${resolvers.pathToStringWithType(path)}', " +
          "please ensure the implementing children are case classes or case objects"
        )
        case (Valid(None), Some(v)) => v.validNel[ValidationFailure]
        case (Valid(Some(v)), _) => gen.from(v).validNel[ValidationFailure]
        case (x @ Invalid(_), _) => x
      }
    )
}

object Extruder {
  object readConfig extends Poly1 {
    implicit def caseAny[A <: Symbol, B](implicit resolver: Resolver[B]): Case.Aux[((A, Seq[String]), Option[B]), ConfigValidation[B]] =
      at[((A, Seq[String]), Option[B])]{ case ((key, prefix), default) => resolver.read(prefix :+ key.name, default) }
  }

  object folder extends Poly2 {
    implicit def caseHList[A, B <: HList]: Case.Aux[ConfigValidation[A], ConfigValidation[B], ConfigValidation[A :: B]] =
      at[ConfigValidation[A], ConfigValidation[B]]((a, b) => (a |@| b).map((aa, bb) => aa :: bb))
  }
}
