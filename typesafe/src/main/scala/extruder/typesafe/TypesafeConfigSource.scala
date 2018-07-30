package extruder.typesafe

import cats.{Monoid, Traverse}
import cats.instances.either._
import cats.instances.list._
import cats.instances.option._
import cats.syntax.either._
import cats.syntax.flatMap._
import cats.syntax.functor._
import com.typesafe.config
import com.typesafe.config.ConfigException.Missing
import com.typesafe.config.{ConfigFactory, ConfigList, ConfigObject, ConfigValue, Config => TConfig}
import extruder.core._
import extruder.effect.{ExtruderMonadError, ExtruderSync}
import extruder.typesafe.IntermediateTypes._
import shapeless.Refute

import scala.collection.JavaConverters._
import scala.collection.TraversableOnce
import scala.collection.generic.CanBuildFrom

trait TypesafeConfigCommon { self: DataSource =>
  override type Sett = Settings

  override val defaultSettings: Settings = new Settings {
    val kebabCaseTransformation: String => String =
      _.replaceAll("([A-Z]+)([A-Z][a-z])", "$1-$2").replaceAll("([a-z\\d])([A-Z])", "$1-$2").toLowerCase

    override def pathToString(path: List[String]): String = path.map(kebabCaseTransformation).mkString(".")
    override val includeClassNameInPath: Boolean = false
  }
}

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
    with StringMapDecoders
    with DerivedDecoders
    with DecodeTypes
    with TypesafeConfigCommon {
  override type InputData = TConfig
  override type DecodeData = TConfig
  override type Dec[F[_], T] = TypesafeConfigDecoder[F, T]

  def lookup[T, F[_]](f: TConfig => T, data: TConfig)(implicit F: Eff[F]): F[Option[T]] =
    F.recoverWith(F.map[T, Option[T]](F.catchNonFatal(f(data)))(Some(_))) {
      case _: Missing => F.pure(None)
    }

  override protected def prepareInput[F[_]](namespace: List[String], settings: Settings, data: TConfig)(
    implicit F: Eff[F]
  ): F[TConfig] =
    F.pure(data)

  override protected def hasValue[F[_]](path: List[String], settings: Settings, data: TConfig)(
    implicit F: Eff[F]
  ): F[Boolean] =
    F.catchNonFatal(data.hasPath(settings.pathToString(path)))

  override protected def lookupValue[F[_]](path: List[String], settings: Settings, data: TConfig)(
    implicit F: Eff[F]
  ): F[Option[String]] =
    lookup(_.getString(settings.pathToString(path)), data)

  override protected def prune[F[_]](path: List[String], settings: Settings, data: TConfig)(
    implicit F: Eff[F]
  ): F[Option[(List[String], config.Config)]] =
    lookup[ConfigObject, F](_.getObject(settings.pathToString(path)), data).map(_.map { co =>
      val pruned =
        co.asScala.map { case (k, v) => s"${settings.pathToString(path)}.$k" -> v }

      co.asScala.keys.toList -> ConfigFactory.parseMap(pruned.asJava)
    })

  private def resolve[F[_], T](
    lookup: (List[String], Settings, TConfig) => F[Option[T]]
  )(implicit F: Eff[F]): (List[String], Settings, Option[T], TConfig) => F[T] =
    resolve[F, T, T](F.pure, lookup)

  implicit def traversableObjectDecoder[F[_], T, FF[T] <: TraversableOnce[T]](
    implicit parser: Parser[List[String]],
    decoder: Dec[F, T],
    cbf: CanBuildFrom[FF[T], T, FF[T]],
    F: Eff[F],
    refute: Refute[Parser[T]]
  ): Dec[F, FF[T]] =
    mkDecoder[F, FF[T]] { (path, settings, default, data) =>
      lookup[List[TConfig], F](_.getConfigList(settings.pathToString(path)).asScala.toList, data)
        .map(_.map(_.map(decoder.read(List.empty, settings, None, _))))
        .flatMap(
          v =>
            (v, default) match {
              case (None, None) =>
                F.missing(s"Could not find list at '${settings.pathToString(path)}' and no default available")
              case (Some(li), _) => Traverse[List].sequence[F, T](li).map(Parser.convertTraversable(_))
              case (None, Some(li)) => F.pure(li)
          }
        )
    }

  implicit def traversableDecoder[T, F[_], FF[T] <: TraversableOnce[T]](
    implicit parser: Parser[T],
    cbf: CanBuildFrom[FF[T], T, FF[T]],
    F: Eff[F]
  ): TypesafeConfigDecoder[F, FF[T]] =
    mkDecoder[F, FF[T]](
      (path, settings: Settings, default, data) =>
        lookup[List[String], F](_.getStringList(settings.pathToString(path)).asScala.toList, data).flatMap(
          listOpt =>
            (listOpt, default) match {
              case (None, None) =>
                F.missing(s"Could not find list at '${settings.pathToString(path)}' and no default available")
              case (None, Some(li)) => F.pure(li)
              case (Some(li), _) =>
                Traverse[List]
                  .sequence[Either[String, ?], T](li.map(parser.parse))
                  .map(Parser.convertTraversable(_))
                  .fold(
                    err =>
                      F.validationFailure(
                        s"Could not parse value '${li.mkString(", ")}' at '${settings.pathToString(path)}': $err"
                    ),
                    F.pure(_)
                  )
          }
      )
    )

  implicit def dataValueDecoder[F[_]](implicit F: Eff[F]): TypesafeConfigDecoder[F, ConfigValue] =
    mkDecoder(resolve[F, ConfigValue]((path, settings, data) => lookup(_.getValue(settings.pathToString(path)), data)))

  implicit def dataListDecoder[F[_]](implicit F: Eff[F]): TypesafeConfigDecoder[F, ConfigList] =
    mkDecoder(resolve[F, ConfigList]((path, settings, data) => lookup(_.getList(settings.pathToString(path)), data)))

  implicit def dataObjectDecoder[F[_]](implicit F: Eff[F]): TypesafeConfigDecoder[F, ConfigObject] =
    mkDecoder(
      resolve[F, ConfigObject]((path, settings, data) => lookup(_.getObject(settings.pathToString(path)), data))
    )

  override def mkDecoder[F[_], T](
    f: (List[String], Settings, Option[T], TConfig) => F[T]
  ): TypesafeConfigDecoder[F, T] =
    new TypesafeConfigDecoder[F, T] {
      override def read(path: List[String], settings: Settings, default: Option[T], data: TConfig): F[T] =
        f(path, settings, default, data)
    }

  override def loadInput[F[_]](implicit F: Eff[F]): F[TConfig] = F.catchNonFatal(ConfigFactory.load())
}

trait TypesafeConfigDecoder[F[_], T] extends Decoder[F, Settings, T, TConfig]

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
    with StringMapEncoders
    with DerivedEncoders
    with EncodeTypes
    with TypesafeConfigCommon {
  override type OutputData = TConfig
  override type EncodeData = Config
  override type Enc[F[_], T] = TypesafeConfigEncoder[F, T]

  override protected val monoid: Monoid[Config] = BaseTypesafeConfigEncoders.monoid

  override protected def writeValue[F[_]](path: List[String], settings: Settings, value: String)(
    implicit F: Eff[F]
  ): F[Config] =
    F.pure(List(ConfigTypes(settings.pathToString(path), value)))

  override protected def finalizeOutput[F[_]](namespace: List[String], settings: Settings, inter: Config)(
    implicit F: Eff[F]
  ): F[TConfig] =
    F.catchNonFatal(inter.map(toConfig).foldLeft[TConfig](ConfigFactory.empty())((a, b) => b.withFallback(a)))

  implicit def traversableObjectEncoder[F[_], T, FF[T] <: TraversableOnce[T]](
    implicit encoder: Enc[F, T],
    refute: Refute[Show[T]],
    F: Eff[F]
  ): TypesafeConfigEncoder[F, FF[T]] = mkEncoder[F, FF[T]] { (path, settings, value) =>
    Traverse[List]
      .sequence[F, Config](value.toList.map(encoder.write(List.empty, settings, _)))
      .map(c => List(ConfigTypes.nested(settings.pathToString(path), c)))
  }

  implicit def traversableEncoder[F[_], T, FF[T] <: TraversableOnce[T]](
    implicit shows: Show[T],
    F: Eff[F]
  ): TypesafeConfigEncoder[F, FF[T]] =
    mkEncoder { (path, settings, value) =>
      F.pure(List(ConfigTypes(settings.pathToString(path), value.map(shows.show).toList)))
    }

  implicit def dataValueEncoder[F[_]](implicit F: Eff[F]): TypesafeConfigEncoder[F, ConfigValue] =
    mkEncoder { (path, settings, value) =>
      F.pure(List(ConfigTypes(settings.pathToString(path), value)))
    }

  implicit def dataListEncoder[F[_]](implicit F: Eff[F]): TypesafeConfigEncoder[F, ConfigList] =
    mkEncoder { (path, settings, value) =>
      F.pure(List(ConfigTypes(settings.pathToString(path), value)))
    }

  implicit def dataObjectEncoder[F[_]](implicit F: Eff[F]): TypesafeConfigEncoder[F, ConfigObject] =
    mkEncoder { (path, settings, value) =>
      F.pure(List(ConfigTypes(settings.pathToString(path), value)))
    }

  override def mkEncoder[F[_], T](f: (List[String], Settings, T) => F[Config]): TypesafeConfigEncoder[F, T] =
    new TypesafeConfigEncoder[F, T] {
      override def write(path: List[String], settings: Settings, in: T): F[Config] = f(path, settings, in)
    }
}

object BaseTypesafeConfigEncoders {
  implicit val monoid: Monoid[Config] = new Monoid[Config] {
    override def empty: Config = List.empty
    override def combine(x: Config, y: Config): Config = x ++ y
  }
}

trait TypesafeConfigEncoder[F[_], T] extends Encoder[F, Settings, T, Config]

object TypesafeConfigEncoder extends TypesafeConfigEncoders

object TypesafeConfigSource extends TypesafeConfigDecoders with TypesafeConfigEncoders

object SafeTypesafeConfigSource extends SafeTypesafeConfigDecoders with SafeTypesafeConfigEncoders
