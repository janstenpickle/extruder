package extruder.typesafe

import cats.instances.either._
import cats.instances.list._
import cats.syntax.either._
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.{ApplicativeError, Monad, MonadError, Traverse}
import com.typesafe.config.ConfigException.Missing
import com.typesafe.config._
import extruder.core.{LoadInput, _}
import extruder.data.PathElement
import shapeless.Refute

import scala.collection.JavaConverters._
import scala.collection.TraversableOnce
import scala.collection.generic.CanBuildFrom

trait TypesafeConfigDecoderInstances extends Resolve { outer =>
  protected def lookup[F[_], A](f: Config => A, data: Config)(
    implicit F: ApplicativeError[F, Throwable]
  ): F[Option[A]] =
    F.recoverWith(F.map[A, Option[A]](F.catchNonFatal(f(data)))(Some(_))) {
      case _: Missing => F.pure(None)
    }

  protected def resolveConfig[F[_], A](
    lookup: (List[PathElement], Settings, Config) => F[Option[A]]
  )(implicit F: Monad[F], error: ExtruderErrors[F]): (List[PathElement], Settings, Option[A], Config) => F[A] =
    resolve[F, A, A, Settings, Config](F.pure, lookup)

  implicit def dataObjectDecoder[F[_]](
    implicit F: MonadError[F, Throwable],
    error: ExtruderErrors[F]
  ): DecoderT[F, Settings, ConfigObject, Config] =
    DecoderT.make[F, Settings, ConfigObject, Config](
      resolveConfig[F, ConfigObject](
        (path, settings, data) => lookup(_.getObject(settings.pathElementListToString(path)), data)
      )
    )

  implicit def dataListDecoder[F[_]](
    implicit F: MonadError[F, Throwable],
    error: ExtruderErrors[F]
  ): DecoderT[F, Settings, ConfigList, Config] =
    DecoderT.make[F, Settings, ConfigList, Config](
      resolveConfig[F, ConfigList](
        (path, settings, data) => lookup(_.getList(settings.pathElementListToString(path)), data)
      )
    )

  implicit def dataValueDecoder[F[_]](
    implicit F: MonadError[F, Throwable],
    error: ExtruderErrors[F]
  ): DecoderT[F, Settings, ConfigValue, Config] =
    DecoderT.make[F, Settings, ConfigValue, Config](
      resolveConfig[F, ConfigValue](
        (path, settings, data) => lookup(_.getValue(settings.pathElementListToString(path)), data)
      )
    )

  implicit def traversableDecoder[F[_], A, FF[T] <: TraversableOnce[T], S <: Settings](
    implicit parser: Parser[A],
    cbf: CanBuildFrom[FF[A], A, FF[A]],
    F: MonadError[F, Throwable],
    error: ExtruderErrors[F]
  ): DecoderT[F, S, FF[A], Config] =
    DecoderT.make[F, S, FF[A], Config](
      (path, settings: Settings, default, data) =>
        lookup[F, List[String]](_.getStringList(settings.pathElementListToString(path)).asScala.toList, data).flatMap(
          listOpt =>
            (listOpt, default) match {
              case (None, None) =>
                error.missing(
                  s"Could not find value at '${settings.pathElementListToString(path)}' and no default available"
                )
              case (None, Some(li)) => F.pure(li)
              case (Some(li), _) =>
                Traverse[List]
                  .traverse[Either[String, ?], String, A](li)(parser.parse)
                  .map(Parser.convertTraversable(_))
                  .fold(
                    err =>
                      error.validationFailure(
                        s"Could not parse value '${li.mkString(", ")}' at '${settings.pathElementListToString(path)}': $err"
                    ),
                    F.pure(_)
                  )
          }
      )
    )

  implicit def traversableObjectDecoder[F[_], A, FF[T] <: TraversableOnce[T], S <: Settings](
    implicit parser: Parser[List[String]],
    decoder: DecoderT[F, S, A, Config],
    cbf: CanBuildFrom[FF[A], A, FF[A]],
    F: MonadError[F, Throwable],
    error: ExtruderErrors[F],
    refute: Refute[Parser[A]]
  ): DecoderT[F, S, FF[A], Config] =
    DecoderT.make[F, S, FF[A], Config] { (path, settings, default, data) =>
      lookup[F, List[Config]](_.getConfigList(settings.pathElementListToString(path)).asScala.toList, data)
        .map(_.map(_.map(decoder.read(List.empty, settings, None, _))))
        .flatMap(
          v =>
            (v, default) match {
              case (None, None) =>
                error.missing(
                  s"Could not find value at '${settings.pathElementListToString(path)}' and no default available"
                )
              case (Some(li), _) => Traverse[List].sequence[F, A](li).map(Parser.convertTraversable(_))
              case (None, Some(li)) => F.pure(li)
          }
        )
    }

  implicit def typesafeConfigLoadInput[F[_]](implicit F: MonadError[F, Throwable]): LoadInput[F, Config] =
    new LoadInput[F, Config] {
      override def load: F[Config] = F.catchNonFatal(ConfigFactory.load())
    }

  implicit def typesafeConfigPrune[F[_]](implicit F: MonadError[F, Throwable]): Prune[F, Settings, Config] =
    new Prune[F, Settings, Config] {
      override def prune(path: List[PathElement], settings: Settings, data: Config): F[Option[(List[String], Config)]] =
        lookup[F, ConfigObject](_.getObject(settings.pathElementListToString(path)), data).map(_.map { co =>
          val pruned =
            co.asScala.map { case (k, v) => s"${settings.pathElementListToString(path)}.$k" -> v }

          co.asScala.keys.toList -> ConfigFactory.parseMap(pruned.asJava)
        })
    }

  implicit def typesafeConfigStringReader[F[_]](
    implicit F: ApplicativeError[F, Throwable]
  ): StringReader[F, Settings, Config] =
    new StringReader[F, Settings, Config] {
      override def lookup(path: List[PathElement], settings: Settings, data: Config): F[Option[String]] =
        outer.lookup[F, String](_.getString(settings.pathElementListToString(path)), data)
    }

  implicit def typesafeConfigHasValue[F[_]](implicit F: ApplicativeError[F, Throwable]): HasValue[F, Settings, Config] =
    new HasValue[F, Settings, Config] {
      override def hasValue(path: List[PathElement], settings: Settings, data: Config): F[Boolean] =
        F.catchNonFatal(data.hasPath(settings.pathElementListToString(path)))
    }
}
