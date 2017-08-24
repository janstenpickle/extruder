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
    with DecodeFromDefaultSource
    with Decoders
    with PrimitiveDecoders
    with DerivedDecoders
    with DecodeTypes {
  override type InputData = TConfig
  override type DecodeData = TConfig
  override type Hint = TypesafeConfigHints
  override type Dec[F[_], T] = TypesafeConfigDecoder[F, T]

  private def lookup[T, F[_], E](f: TConfig => T, path: List[String], data: TConfig)(
    implicit hints: Hint,
    AE: ExtruderApplicativeError[F, E]
  ): IO[F[Option[T]]] = IO {
    Either
      .catchNonFatal(f(data))
      .fold(
        {
          case _: Missing => AE.pure(None)
          case th: Any =>
            AE.validationException(
              s"Could not retrieve config '${hints.pathToString(path)}' from supplied Typesafe config",
              th
            )
        },
        v => AE.pure(Some(v))
      )
  }

  override protected def prepareInput[F[_], E](
    namespace: List[String],
    data: TConfig
  )(implicit AE: ExtruderApplicativeError[F, E], hints: Hint): IO[F[TConfig]] =
    IO(AE.pure(data))

  override protected def hasValue[F[_], E](
    path: List[String],
    data: TConfig
  )(implicit hints: TypesafeConfigHints, AE: ExtruderApplicativeError[F, E]): IO[F[Boolean]] =
    lookup[ConfigValue, F, E](_.getValue(hints.pathToString(path)), path, data).map(AE.map(_)(_.isDefined))

  override protected def lookupValue[F[_], E](
    path: List[String],
    data: TConfig
  )(implicit hints: Hint, AE: ExtruderApplicativeError[F, E]): IO[F[Option[String]]] =
    lookup(_.getString(hints.pathToString(path)), path, data)

  override protected def lookupList[F[_], E](
    path: List[String],
    data: TConfig
  )(implicit hints: Hint, AE: ExtruderApplicativeError[F, E]): IO[F[Option[List[String]]]] =
    lookup(_.getStringList(hints.pathToString(path)).asScala.toList, path, data)

  private def resolve[F[_], E, T](
    lookup: (List[String], TConfig) => IO[F[Option[T]]]
  )(implicit AE: ExtruderApplicativeError[F, E]): (List[String], Option[T], TConfig) => IO[F[T]] =
    resolve[F, E, T, T](AE.pure, lookup)

  implicit def dataValueDecoder[F[_], E](
    implicit hints: Hint,
    AE: ExtruderApplicativeError[F, E]
  ): TypesafeConfigDecoder[F, ConfigValue] =
    mkDecoder(resolve[F, E, ConfigValue]((path, data) => lookup(_.getValue(hints.pathToString(path)), path, data)))

  implicit def dataListDecoder[F[_], E](
    implicit hints: Hint,
    AE: ExtruderApplicativeError[F, E]
  ): TypesafeConfigDecoder[F, ConfigList] =
    mkDecoder(resolve[F, E, ConfigList]((path, data) => lookup(_.getList(hints.pathToString(path)), path, data)))

  implicit def dataObjectDecoder[F[_], E](
    implicit hints: Hint,
    AE: ExtruderApplicativeError[F, E]
  ): TypesafeConfigDecoder[F, ConfigObject] =
    mkDecoder(resolve[F, E, ConfigObject]((path, data) => lookup(_.getObject(hints.pathToString(path)), path, data)))

  override def mkDecoder[F[_], T](f: (List[String], Option[T], TConfig) => IO[F[T]]): TypesafeConfigDecoder[F, T] =
    new TypesafeConfigDecoder[F, T] {
      override def read(path: List[String], default: Option[T], data: TConfig): IO[F[T]] = f(path, default, data)
    }

  override def loadInput: IO[TConfig] = IO(ConfigFactory.load())
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

  private def valueToConfig[F[_], E](
    path: List[String],
    value: ConfigTypes
  )(implicit hints: Hint, AE: ExtruderApplicativeError[F, E]): IO[F[ConfigMap]] =
    IO(AE.pure(Map(hints.pathToString(path) -> value)))

  override protected def writeValue[F[_], E](
    path: List[String],
    value: String
  )(implicit hints: Hint, AE: ExtruderApplicativeError[F, E]): IO[F[ConfigMap]] =
    valueToConfig(path, Coproduct[ConfigTypes](value))

  override protected def finalizeOutput[F[_], E](
    namespace: List[String],
    inter: ConfigMap
  )(implicit AE: ExtruderApplicativeError[F, E], hints: Hint): IO[F[TConfig]] =
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
    hints: Hint,
    AE: ExtruderApplicativeError[F, E]
  ): TypesafeConfigEncoder[F, FF[T]] =
    mkEncoder { (path, value) =>
      IO(AE.pure(Map(hints.pathToString(path) -> Coproduct[ConfigTypes](value.map(shows.show).toList))))
    }

  implicit def dataValueEncoder[F[_], E](
    implicit hints: Hint,
    AE: ExtruderApplicativeError[F, E]
  ): TypesafeConfigEncoder[F, ConfigValue] =
    mkEncoder { (path, value) =>
      IO(AE.pure(Map(hints.pathToString(path) -> Coproduct[ConfigTypes](value))))
    }

  implicit def dataListEncoder[F[_], E](
    implicit hints: Hint,
    AE: ExtruderApplicativeError[F, E]
  ): TypesafeConfigEncoder[F, ConfigList] =
    mkEncoder { (path, value) =>
      IO(AE.pure(Map(hints.pathToString(path) -> Coproduct[ConfigTypes](value))))
    }

  implicit def dataObjectEncoder[F[_], E](
    implicit hints: Hint,
    AE: ExtruderApplicativeError[F, E]
  ): TypesafeConfigEncoder[F, ConfigObject] =
    mkEncoder { (path, value) =>
      IO(AE.pure(Map(hints.pathToString(path) -> Coproduct[ConfigTypes](value))))
    }

  override def mkEncoder[F[_], T](f: (List[String], T) => IO[F[ConfigMap]]): TypesafeConfigEncoder[F, T] =
    new TypesafeConfigEncoder[F, T] {
      override def write(path: List[String], in: T): IO[F[ConfigMap]] = f(path, in)
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

    override def prependtoPath(prepend: String, path: String) = s"${dashTransformation(prepend)}.$path"
  }
}
