package extruder.typesafe

import cats.Monoid
import com.typesafe.config.ConfigException.Missing
import com.typesafe.config.{ConfigFactory, ConfigList, ConfigObject, ConfigValue, Config => TConfig}
import extruder.core._
import extruder.effect.ExtruderAsync
import shapeless.Coproduct

import scala.collection.JavaConverters._

trait TypesafeConfigDecoders
    extends Decode
    with DecodeFromDefaultSource
    with Decoders
    with PrimitiveDecoders
    with DerivedDecoders
    with DecodeTypes {
  override type InputData = TConfig
  override type DecodeData = TConfig
  override type Hint = TypesafeConfigHints
  override type Dec[F[_], T] = TypesafeConfigDecoder[F, T]

  private def lookup[T, F[_]](f: TConfig => T, path: List[String], data: TConfig)(
    implicit hints: Hint,
    F: ExtruderAsync[F]
  ): F[Option[T]] =
    F.recoverWith(F.map[T, Option[T]](F.catchNonFatal(f(data)))(Some(_))) {
      case _: Missing => F.pure(None)
    }

  override protected def prepareInput[F[_]](
    namespace: List[String],
    data: TConfig
  )(implicit F: ExtruderAsync[F], hints: Hint): F[TConfig] =
    F.pure(data)

  override protected def hasValue[F[_]](
    path: List[String],
    data: TConfig
  )(implicit hints: TypesafeConfigHints, F: ExtruderAsync[F]): F[Boolean] =
    F.map(lookup[ConfigValue, F](_.getValue(hints.pathToString(path)), path, data))(_.isDefined)

  override protected def lookupValue[F[_]](
    path: List[String],
    data: TConfig
  )(implicit hints: Hint, F: ExtruderAsync[F]): F[Option[String]] =
    lookup(_.getString(hints.pathToString(path)), path, data)

  override protected def lookupList[F[_]](
    path: List[String],
    data: TConfig
  )(implicit hints: Hint, F: ExtruderAsync[F]): F[Option[List[String]]] =
    lookup(_.getStringList(hints.pathToString(path)).asScala.toList, path, data)

  private def resolve[F[_], T](
    lookup: (List[String], TConfig) => F[Option[T]]
  )(implicit F: ExtruderAsync[F]): (List[String], Option[T], TConfig) => F[T] =
    resolve[F, T, T](F.pure, lookup)

  implicit def dataValueDecoder[F[_]](
    implicit hints: Hint,
    F: ExtruderAsync[F]
  ): TypesafeConfigDecoder[F, ConfigValue] =
    mkDecoder(resolve[F, ConfigValue]((path, data) => lookup(_.getValue(hints.pathToString(path)), path, data)))

  implicit def dataListDecoder[F[_]](implicit hints: Hint, F: ExtruderAsync[F]): TypesafeConfigDecoder[F, ConfigList] =
    mkDecoder(resolve[F, ConfigList]((path, data) => lookup(_.getList(hints.pathToString(path)), path, data)))

  implicit def dataObjectDecoder[F[_]](
    implicit hints: Hint,
    F: ExtruderAsync[F]
  ): TypesafeConfigDecoder[F, ConfigObject] =
    mkDecoder(resolve[F, ConfigObject]((path, data) => lookup(_.getObject(hints.pathToString(path)), path, data)))

  override def mkDecoder[F[_], T](f: (List[String], Option[T], TConfig) => F[T]): TypesafeConfigDecoder[F, T] =
    new TypesafeConfigDecoder[F, T] {
      override def read(path: List[String], default: Option[T], data: TConfig): F[T] = f(path, default, data)
    }

  override def loadInput[F[_]](implicit F: ExtruderAsync[F]): F[TConfig] = F.delay(ConfigFactory.load())
}

trait TypesafeConfigDecoder[F[_], T] extends Decoder[F, T, TConfig]

object TypesafeConfigDecoder extends TypesafeConfigDecoders

trait TypesafeConfigEncoders
    extends Encode
    with Encoders
    with PrimitiveEncoders
    with DerivedEncoders
    with EncodeTypes {
  override type OutputData = TConfig
  override type EncodeData = ConfigMap
  override type Enc[F[_], T] = TypesafeConfigEncoder[F, T]
  override type Hint = TypesafeConfigHints

  override protected val monoid: Monoid[ConfigMap] = new Monoid[ConfigMap] {
    override def empty: ConfigMap = Map.empty
    override def combine(x: ConfigMap, y: ConfigMap): ConfigMap = x ++ y
  }

  private def valueToConfig[F[_]](
    path: List[String],
    value: ConfigTypes
  )(implicit hints: Hint, F: ExtruderAsync[F]): F[ConfigMap] =
    F.pure(Map(hints.pathToString(path) -> value))

  override protected def writeValue[F[_]](
    path: List[String],
    value: String
  )(implicit hints: Hint, F: ExtruderAsync[F]): F[ConfigMap] =
    valueToConfig(path, Coproduct[ConfigTypes](value))

  override protected def finalizeOutput[F[_]](
    namespace: List[String],
    inter: ConfigMap
  )(implicit F: ExtruderAsync[F], hints: Hint): F[TConfig] =
    F.catchNonFatal(ConfigFactory.parseMap(inter.flatMap {
      case (k, v) =>
        (v.select[String].toList ++
          v.select[ConfigValue] ++
          v.select[ConfigList] ++
          v.select[ConfigObject] ++
          v.select[List[String]].map(_.asJava)).map(k -> _)
    }.asJava))

  override implicit def traversableEncoder[F[_], T, FF[T] <: TraversableOnce[T]](
    implicit shows: Show[T],
    hints: Hint,
    F: ExtruderAsync[F]
  ): TypesafeConfigEncoder[F, FF[T]] =
    mkEncoder { (path, value) =>
      F.pure(Map(hints.pathToString(path) -> Coproduct[ConfigTypes](value.map(shows.show).toList)))
    }

  implicit def dataValueEncoder[F[_]](
    implicit hints: Hint,
    F: ExtruderAsync[F]
  ): TypesafeConfigEncoder[F, ConfigValue] =
    mkEncoder { (path, value) =>
      F.pure(Map(hints.pathToString(path) -> Coproduct[ConfigTypes](value)))
    }

  implicit def dataListEncoder[F[_]](implicit hints: Hint, F: ExtruderAsync[F]): TypesafeConfigEncoder[F, ConfigList] =
    mkEncoder { (path, value) =>
      F.pure(Map(hints.pathToString(path) -> Coproduct[ConfigTypes](value)))
    }

  implicit def dataObjectEncoder[F[_]](
    implicit hints: Hint,
    F: ExtruderAsync[F]
  ): TypesafeConfigEncoder[F, ConfigObject] =
    mkEncoder { (path, value) =>
      F.pure(Map(hints.pathToString(path) -> Coproduct[ConfigTypes](value)))
    }

  override def mkEncoder[F[_], T](f: (List[String], T) => F[ConfigMap]): TypesafeConfigEncoder[F, T] =
    new TypesafeConfigEncoder[F, T] {
      override def write(path: List[String], in: T): F[ConfigMap] = f(path, in)
    }
}

trait TypesafeConfigEncoder[F[_], T] extends Encoder[F, T, ConfigMap]

object TypesafeConfigEncoder extends TypesafeConfigEncoders

object TypesafeConfigSource extends TypesafeConfigDecoders with TypesafeConfigEncoders

trait TypesafeConfigHints extends Hints

object TypesafeConfigHints extends HintsCompanion[TypesafeConfigHints] {
  override implicit val default: TypesafeConfigHints = new TypesafeConfigHints {
    val dashTransformation: String => String =
      _.replaceAll("([A-Z]+)([A-Z][a-z])", "$1-$2").replaceAll("([a-z\\d])([A-Z])", "$1-$2").toLowerCase

    override def pathToString(path: List[String]): String = path.map(dashTransformation).mkString(".")
  }
}
