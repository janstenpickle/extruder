package extruder.typesafe

import cats.instances.all._
import cats.{Applicative, Monoid}
import cats.syntax.either._
import com.typesafe.config.ConfigException.Missing
import com.typesafe.config.{ConfigFactory, ConfigList, ConfigObject, ConfigValue, Config => TConfig}
import extruder.core._
import extruder.effect.{ExtruderMonadError, ExtruderSync}
import shapeless.Coproduct

import scala.collection.JavaConverters._
import scala.collection.TraversableOnce
import scala.collection.generic.CanBuildFrom

trait TypesafeConfigDecoders extends BaseTypesafeConfigDecoders {
  override type Eff[F[_]] = ExtruderMonadError[F]
}

trait SafeTypesafeConfigDecoders extends BaseTypesafeConfigDecoders {
  override type Eff[F[_]] = ExtruderSync[F]

  override def loadInput[F[_]](implicit F: Eff[F]): F[TConfig] = F.delay(ConfigFactory.load())
}

trait BaseTypesafeConfigDecoders
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

  protected def lookup[T, F[_]](f: TConfig => T, data: TConfig)(implicit hints: Hint, F: Eff[F]): F[Option[T]] =
    F.recoverWith(F.map[T, Option[T]](F.catchNonFatal(f(data)))(Some(_))) {
      case _: Missing => F.pure(None)
    }

  override protected def prepareInput[F[_]](
    namespace: List[String],
    data: TConfig
  )(implicit F: Eff[F], hints: Hint): F[TConfig] =
    F.pure(data)

  override protected def hasValue[F[_]](
    path: List[String],
    data: TConfig
  )(implicit hints: TypesafeConfigHints, F: Eff[F]): F[Boolean] =
    F.map(lookup[ConfigValue, F](_.getValue(hints.pathToString(path)), data))(_.isDefined)

  override protected def lookupValue[F[_]](
    path: List[String],
    data: TConfig
  )(implicit hints: Hint, F: Eff[F]): F[Option[String]] =
    lookup(_.getString(hints.pathToString(path)), data)

  private def resolve[F[_], T](
    lookup: (List[String], TConfig) => F[Option[T]]
  )(implicit F: Eff[F]): (List[String], Option[T], TConfig) => F[T] =
    resolve[F, T, T](F.pure, lookup)

  implicit def traversableDecoder[T, F[_], FF[T] <: TraversableOnce[T]](
    implicit hints: Hint,
    parser: Parser[T],
    cbf: CanBuildFrom[List[T], T, FF[T]],
    F: Eff[F]
  ): TypesafeConfigDecoder[F, FF[T]] =
    mkDecoder[F, FF[T]](
      (path, default, data) =>
        F.flatMap(lookup[List[String], F](_.getStringList(hints.pathToString(path)).asScala.toList, data))(
          listOpt =>
            (listOpt, default) match {
              case (None, None) =>
                F.missing(s"Could not find list at '${hints.pathToString(path)}' and no default available")
              case (None, Some(li)) => F.pure(li)
              case (Some(li), _) =>
                Applicative[Either[String, ?]]
                  .sequence(li.map(parser.parse))
                  .map(_.map[T, FF[T]](identity))
                  .fold(
                    err =>
                      F.validationFailure(
                        s"Could not parse value '${li.toString()}' at '${hints.pathToString(path)}': $err"
                    ),
                    F.pure
                  )
          }
      )
    )

  implicit def dataValueDecoder[F[_]](implicit hints: Hint, F: Eff[F]): TypesafeConfigDecoder[F, ConfigValue] =
    mkDecoder(resolve[F, ConfigValue]((path, data) => lookup(_.getValue(hints.pathToString(path)), data)))

  implicit def dataListDecoder[F[_]](implicit hints: Hint, F: Eff[F]): TypesafeConfigDecoder[F, ConfigList] =
    mkDecoder(resolve[F, ConfigList]((path, data) => lookup(_.getList(hints.pathToString(path)), data)))

  implicit def dataObjectDecoder[F[_]](implicit hints: Hint, F: Eff[F]): TypesafeConfigDecoder[F, ConfigObject] =
    mkDecoder(resolve[F, ConfigObject]((path, data) => lookup(_.getObject(hints.pathToString(path)), data)))

  override def mkDecoder[F[_], T](f: (List[String], Option[T], TConfig) => F[T]): TypesafeConfigDecoder[F, T] =
    new TypesafeConfigDecoder[F, T] {
      override def read(path: List[String], default: Option[T], data: TConfig): F[T] = f(path, default, data)
    }

  override def loadInput[F[_]](implicit F: Eff[F]): F[TConfig] = F.catchNonFatal(ConfigFactory.load())
}

trait TypesafeConfigDecoder[F[_], T] extends Decoder[F, T, TConfig]

object TypesafeConfigDecoder extends TypesafeConfigDecoders

trait TypesafeConfigEncoders extends BaseTypesafeConfigEncoders {
  override type Eff[F[_]] = ExtruderMonadError[F]
}

trait SafeTypesafeConfigEncoders extends BaseTypesafeConfigEncoders {
  override type Eff[F[_]] = ExtruderSync[F]
}

trait BaseTypesafeConfigEncoders
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
  )(implicit hints: Hint, F: Eff[F]): F[ConfigMap] =
    F.pure(Map(hints.pathToString(path) -> value))

  override protected def writeValue[F[_]](
    path: List[String],
    value: String
  )(implicit hints: Hint, F: Eff[F]): F[ConfigMap] =
    valueToConfig(path, Coproduct[ConfigTypes](value))

  override protected def finalizeOutput[F[_]](
    namespace: List[String],
    inter: ConfigMap
  )(implicit F: Eff[F], hints: Hint): F[TConfig] =
    F.catchNonFatal(ConfigFactory.parseMap(inter.flatMap {
      case (k, v) =>
        (v.select[String].toList ++
          v.select[ConfigValue] ++
          v.select[ConfigList] ++
          v.select[ConfigObject] ++
          v.select[List[String]].map(_.asJava)).map(k -> _)
    }.asJava))

  implicit def traversableEncoder[F[_], T, FF[T] <: TraversableOnce[T]](
    implicit shows: Show[T],
    hints: Hint,
    F: Eff[F]
  ): TypesafeConfigEncoder[F, FF[T]] =
    mkEncoder { (path, value) =>
      F.pure(Map(hints.pathToString(path) -> Coproduct[ConfigTypes](value.map(shows.show).toList)))
    }

  implicit def dataValueEncoder[F[_]](implicit hints: Hint, F: Eff[F]): TypesafeConfigEncoder[F, ConfigValue] =
    mkEncoder { (path, value) =>
      F.pure(Map(hints.pathToString(path) -> Coproduct[ConfigTypes](value)))
    }

  implicit def dataListEncoder[F[_]](implicit hints: Hint, F: Eff[F]): TypesafeConfigEncoder[F, ConfigList] =
    mkEncoder { (path, value) =>
      F.pure(Map(hints.pathToString(path) -> Coproduct[ConfigTypes](value)))
    }

  implicit def dataObjectEncoder[F[_]](implicit hints: Hint, F: Eff[F]): TypesafeConfigEncoder[F, ConfigObject] =
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

object SafeTypesafeConfigSource extends SafeTypesafeConfigDecoders with SafeTypesafeConfigEncoders

trait TypesafeConfigHints extends Hints

object TypesafeConfigHints extends HintsCompanion[TypesafeConfigHints] {
  override implicit val default: TypesafeConfigHints = new TypesafeConfigHints {
    val dashTransformation: String => String =
      _.replaceAll("([A-Z]+)([A-Z][a-z])", "$1-$2").replaceAll("([a-z\\d])([A-Z])", "$1-$2").toLowerCase

    override def pathToString(path: List[String]): String = path.map(dashTransformation).mkString(".")
  }
}
