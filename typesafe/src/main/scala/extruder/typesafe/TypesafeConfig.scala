package extruder.typesafe

import cats.Monoid
import cats.syntax.either._
import cats.syntax.validated._
import com.typesafe.config.ConfigException.Missing
import com.typesafe.config.{ConfigFactory, ConfigList, ConfigObject, ConfigValue, Config => TConfig}
import extruder.core._
import extruder.syntax.validation._
import shapeless.Coproduct

import scala.collection.JavaConverters._

trait TypesafeConfigDecoders extends Decode[TConfig, TConfig, TypesafeConfigDecoder]
                             with Decoders[TConfig, TypesafeConfigDecoder]
                             with PrimitiveDecoders[TConfig, TypesafeConfigDecoder]
                             with DerivedDecoders[TConfig, TypesafeConfigDecoder]
                             with TypesafeConfigUtilsMixin {
  def lookup[T](f: TConfig => T, path: Seq[String], config: TConfig): ConfigValidation[Option[T]] =
    Either.catchNonFatal(f(config)).fold({
      case _ : Missing => None.validNel
      case th: Any => ValidationException(
        s"Could not retrieve config '${utils.pathToString(path)}' from supplied Typesafe config",
        th
      ).invalidNel
    }, Some(_).validNel)

  override protected def prepareConfig(config: TConfig): ConfigValidation[TConfig] =
    config.validNel

  override protected def lookupValue(path: Seq[String], config: TConfig): ConfigValidation[Option[String]] =
    lookup(_.getString(utils.pathToString(path)), path, config)

  override protected def lookupList(path: Seq[String], config: TConfig): ConfigValidation[Option[List[String]]] =
    lookup(_.getStringList(utils.pathToString(path)).asScala.toList, path, config)

  private def resolve[T](lookup: (Seq[String], TConfig) => ConfigValidation[Option[T]]): (Seq[String], Option[T], TConfig) => ConfigValidation[T] =
    resolve[T, T](_.validNel, lookup)

  implicit val configValueDecoder: TypesafeConfigDecoder[ConfigValue] =
    mkDecoder(resolve[ConfigValue]((path, config) => lookup(_.getValue(utils.pathToString(path)), path, config)))

  implicit val configListDecoder: TypesafeConfigDecoder[ConfigList] =
    mkDecoder(resolve[ConfigList]((path, config) => lookup(_.getList(utils.pathToString(path)), path, config)))

  implicit val configObjectDecoder: TypesafeConfigDecoder[ConfigObject] =
    mkDecoder(resolve[ConfigObject]((path, config) => lookup(_.getObject(utils.pathToString(path)), path, config)))

  override def mkDecoder[T](f: (Seq[String], Option[T], TConfig) => ConfigValidation[T]): TypesafeConfigDecoder[T] =
    new TypesafeConfigDecoder[T] {
      override def read(path: Seq[String], default: Option[T], config: TConfig): ConfigValidation[T] = f(path, default, config)
    }

  def decode[T](implicit decoder: TypesafeConfigDecoder[T]): ConfigValidation[T] = decode[T](Seq.empty)

  def decode[T](namespace: Seq[String])(implicit decoder: TypesafeConfigDecoder[T]): ConfigValidation[T] =
    ConfigFactory.load().handle.fold(_.invalid, decode[T](namespace, _))
}

trait TypesafeConfigDecoder[T] extends Decoder[T, TConfig]

object TypesafeConfigDecoder extends TypesafeConfigDecoders

trait TypesafeConfigEncoders extends Encode[ConfigMap, TConfig, TypesafeConfigEncoder]
                             with Encoders[ConfigMap, TypesafeConfigEncoder]
                             with PrimitiveEncoders[ConfigMap, TypesafeConfigEncoder]
                             with DerivedEncoders[ConfigMap, TypesafeConfigEncoder]
                             with TypesafeConfigUtilsMixin {
  override protected val monoid: Monoid[ConfigMap] = new Monoid[ConfigMap] {
    override def empty: ConfigMap = Map.empty
    override def combine(x: ConfigMap, y: ConfigMap): ConfigMap = x ++ y
  }

  private def valueToConfig(path: Seq[String], value: ConfigTypes): ConfigValidation[ConfigMap] =
    Map(utils.pathToString(path) -> value).validNel

  override protected def writeValue(path: Seq[String], value: String): ConfigValidation[ConfigMap] =
    valueToConfig(path, Coproduct[ConfigTypes](value))

  override protected def finalizeConfig(inter: ConfigMap): ConfigValidation[TConfig] =
    ConfigFactory.parseMap(inter.flatMap { case (k, v) =>
      (v.select[String].toSeq ++
       v.select[ConfigValue] ++
       v.select[ConfigList] ++
       v.select[ConfigObject] ++
       v.select[List[String]].map(_.asJava)).map(k.toLowerCase -> _)
    }.asJava).handle

  override implicit def traversableEncoder[T, F[T] <: TraversableOnce[T]](implicit shows: Show[T]): TypesafeConfigEncoder[F[T]] =
    mkEncoder((path, value) => Map(utils.pathToString(path) -> Coproduct[ConfigTypes](value.map(shows.show).toList)).validNel)

  implicit def configValueEncoder: TypesafeConfigEncoder[ConfigValue] =
    mkEncoder((path, value) => Map(utils.pathToString(path) -> Coproduct[ConfigTypes](value)).validNel)

  implicit def configListEncoder: TypesafeConfigEncoder[ConfigList] =
    mkEncoder((path, value) => Map(utils.pathToString(path) -> Coproduct[ConfigTypes](value)).validNel)

  implicit def configObjectEncoder: TypesafeConfigEncoder[ConfigObject] =
    mkEncoder((path, value) => Map(utils.pathToString(path) -> Coproduct[ConfigTypes](value)).validNel)

  override def mkEncoder[T](f: (Seq[String], T) => ConfigValidation[ConfigMap]): TypesafeConfigEncoder[T] =
    new TypesafeConfigEncoder[T] {
      override def write(path: Seq[String], in: T): ConfigValidation[ConfigMap] = f(path, in)
    }
}

trait TypesafeConfigEncoder[T] extends Encoder[T, ConfigMap]

object TypesafeConfigEncoder extends TypesafeConfigEncoders

object TypesafeConfig extends TypesafeConfigDecoders with TypesafeConfigEncoders

trait TypesafeConfigUtils extends Utils

object TypesafeConfigUtils extends UtilsCompanion[TypesafeConfigUtils] {
  override implicit val default: TypesafeConfigUtils = new TypesafeConfigUtils {
    val dashTransformation: String => String = _.replaceAll(
      "([A-Z]+)([A-Z][a-z])",
      "$1-$2"
    ).replaceAll("([a-z\\d])([A-Z])", "$1-$2").toLowerCase

    override def pathToString(path: Seq[String]): String = path.map(dashTransformation).mkString(".")
  }
}

trait TypesafeConfigUtilsMixin extends UtilsMixin {
  override type U = TypesafeConfigUtils
  override val utils: TypesafeConfigUtils = implicitly[TypesafeConfigUtils]
}
