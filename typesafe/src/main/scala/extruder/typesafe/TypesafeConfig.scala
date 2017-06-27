package extruder.typesafe

import cats.Monoid
import cats.effect.IO
import cats.syntax.either._
import com.typesafe.config.ConfigException.Missing
import com.typesafe.config.{ConfigFactory, ConfigList, ConfigObject, ConfigValue, Config => TConfig}
import extruder.core._
import shapeless.Coproduct

import scala.collection.JavaConverters._

trait TypesafeConfigDecoders
    extends Decode
    with DecodeFromDefaultConfig
    with Decoders
    with PrimitiveDecoders
    with DerivedDecoders
    with DecodeTypes {
  override type InputConfig = TConfig
  override type DecodeConfig = TConfig
  override type Hint = TypesafeConfigHints
  override type Dec[F[_], T] = TypesafeConfigDecoder[F, T]

  private def lookup[T, F[_], E](f: TConfig => T, path: List[String], config: TConfig)(
    implicit utils: Hint,
    AE: ExtruderApplicativeError[F, E]
  ): IO[F[Option[T]]] = IO {
    Either
      .catchNonFatal(f(config))
      .fold(
        {
          case _: Missing => AE.pure(None)
          case th: Any =>
            AE.validationException(
              s"Could not retrieve config '${utils.pathToString(path)}' from supplied Typesafe config",
              th
            )
        },
        v => AE.pure(Some(v))
      )
  }

  override protected def prepareConfig[F[_], E](
    namespace: List[String],
    config: TConfig
  )(implicit AE: ExtruderApplicativeError[F, E], utils: Hint): IO[F[TConfig]] =
    IO(AE.pure(config))

  override protected def hasValue[F[_], E](
    path: List[String],
    config: TConfig
  )(implicit utils: TypesafeConfigHints, AE: ExtruderApplicativeError[F, E]): IO[F[Boolean]] =
    lookup[ConfigValue, F, E](_.getValue(utils.pathToString(path)), path, config).map(AE.map(_)(_.isDefined))

  override protected def lookupValue[F[_], E](
    path: List[String],
    config: TConfig
  )(implicit utils: Hint, AE: ExtruderApplicativeError[F, E]): IO[F[Option[String]]] =
    lookup(_.getString(utils.pathToString(path)), path, config)

  override protected def lookupList[F[_], E](
    path: List[String],
    config: TConfig
  )(implicit utils: Hint, AE: ExtruderApplicativeError[F, E]): IO[F[Option[List[String]]]] =
    lookup(_.getStringList(utils.pathToString(path)).asScala.toList, path, config)

  private def resolve[F[_], E, T](
    lookup: (List[String], TConfig) => IO[F[Option[T]]]
  )(implicit AE: ExtruderApplicativeError[F, E]): (List[String], Option[T], TConfig) => IO[F[T]] =
    resolve[F, E, T, T](AE.pure, lookup)

  implicit def configValueDecoder[F[_], E](
    implicit utils: Hint,
    AE: ExtruderApplicativeError[F, E]
  ): TypesafeConfigDecoder[F, ConfigValue] =
    mkDecoder(resolve[F, E, ConfigValue]((path, config) => lookup(_.getValue(utils.pathToString(path)), path, config)))

  implicit def configListDecoder[F[_], E](
    implicit utils: Hint,
    AE: ExtruderApplicativeError[F, E]
  ): TypesafeConfigDecoder[F, ConfigList] =
    mkDecoder(resolve[F, E, ConfigList]((path, config) => lookup(_.getList(utils.pathToString(path)), path, config)))

  implicit def configObjectDecoder[F[_], E](
    implicit utils: Hint,
    AE: ExtruderApplicativeError[F, E]
  ): TypesafeConfigDecoder[F, ConfigObject] =
    mkDecoder(
      resolve[F, E, ConfigObject]((path, config) => lookup(_.getObject(utils.pathToString(path)), path, config))
    )

  override def mkDecoder[F[_], T](f: (List[String], Option[T], TConfig) => IO[F[T]]): TypesafeConfigDecoder[F, T] =
    new TypesafeConfigDecoder[F, T] {
      override def read(path: List[String], default: Option[T], config: TConfig): IO[F[T]] = f(path, default, config)
    }

  override def loadConfig: IO[TConfig] = IO(ConfigFactory.load())
}

trait TypesafeConfigDecoder[F[_], T] extends Decoder[F, T, TConfig]

object TypesafeConfigDecoder extends TypesafeConfigDecoders

trait TypesafeConfigEncoders
    extends Encode
    with Encoders
    with PrimitiveEncoders
    with DerivedEncoders
    with EncodeTypes {
  override type OutputConfig = TConfig
  override type EncodeConfig = ConfigMap
  override type Enc[F[_], T] = TypesafeConfigEncoder[F, T]
  override type Hint = TypesafeConfigHints

  override protected val monoid: Monoid[ConfigMap] = new Monoid[ConfigMap] {
    override def empty: ConfigMap = Map.empty
    override def combine(x: ConfigMap, y: ConfigMap): ConfigMap = x ++ y
  }

  private def valueToConfig[F[_], E](
    path: List[String],
    value: ConfigTypes
  )(implicit utils: Hint, AE: ExtruderApplicativeError[F, E]): IO[F[ConfigMap]] =
    IO(AE.pure(Map(utils.pathToString(path) -> value)))

  override protected def writeValue[F[_], E](
    path: List[String],
    value: String
  )(implicit utils: Hint, AE: ExtruderApplicativeError[F, E]): IO[F[ConfigMap]] =
    valueToConfig(path, Coproduct[ConfigTypes](value))

  override protected def finalizeConfig[F[_], E](
    namespace: List[String],
    inter: ConfigMap
  )(implicit AE: ExtruderApplicativeError[F, E], utils: Hint): IO[F[TConfig]] =
    IO(AE.catchNonFatal(ConfigFactory.parseMap(inter.flatMap {
      case (k, v) =>
        (v.select[String].toList ++
          v.select[ConfigValue] ++
          v.select[ConfigList] ++
          v.select[ConfigObject] ++
          v.select[List[String]].map(_.asJava)).map(k -> _)
    }.asJava)))

  override implicit def traversableEncoder[F[_], E, T, FF[T] <: TraversableOnce[T]](
    implicit shows: Show[T],
    utils: Hint,
    AE: ExtruderApplicativeError[F, E]
  ): TypesafeConfigEncoder[F, FF[T]] =
    mkEncoder { (path, value) =>
      IO(AE.pure(Map(utils.pathToString(path) -> Coproduct[ConfigTypes](value.map(shows.show).toList))))
    }

  implicit def configValueEncoder[F[_], E](
    implicit utils: Hint,
    AE: ExtruderApplicativeError[F, E]
  ): TypesafeConfigEncoder[F, ConfigValue] =
    mkEncoder { (path, value) =>
      IO(AE.pure(Map(utils.pathToString(path) -> Coproduct[ConfigTypes](value))))
    }

  implicit def configListEncoder[F[_], E](
    implicit utils: Hint,
    AE: ExtruderApplicativeError[F, E]
  ): TypesafeConfigEncoder[F, ConfigList] =
    mkEncoder { (path, value) =>
      IO(AE.pure(Map(utils.pathToString(path) -> Coproduct[ConfigTypes](value))))
    }

  implicit def configObjectEncoder[F[_], E](
    implicit utils: Hint,
    AE: ExtruderApplicativeError[F, E]
  ): TypesafeConfigEncoder[F, ConfigObject] =
    mkEncoder { (path, value) =>
      IO(AE.pure(Map(utils.pathToString(path) -> Coproduct[ConfigTypes](value))))
    }

  override def mkEncoder[F[_], T](f: (List[String], T) => IO[F[ConfigMap]]): TypesafeConfigEncoder[F, T] =
    new TypesafeConfigEncoder[F, T] {
      override def write(path: List[String], in: T): IO[F[ConfigMap]] = f(path, in)
    }
}

trait TypesafeConfigEncoder[F[_], T] extends Encoder[F, T, ConfigMap]

object TypesafeConfigEncoder extends TypesafeConfigEncoders

object TypesafeConfig extends TypesafeConfigDecoders with TypesafeConfigEncoders

trait TypesafeConfigHints extends Hints

object TypesafeConfigHints extends HintsCompanion[TypesafeConfigHints] {
  override implicit val default: TypesafeConfigHints = new TypesafeConfigHints {
    val dashTransformation: String => String =
      _.replaceAll("([A-Z]+)([A-Z][a-z])", "$1-$2").replaceAll("([a-z\\d])([A-Z])", "$1-$2").toLowerCase

    override def pathToString(path: List[String]): String = path.map(dashTransformation).mkString(".")
  }
}
