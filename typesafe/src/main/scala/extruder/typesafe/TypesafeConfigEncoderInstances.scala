package extruder.typesafe

import cats.instances.list.{catsKernelStdMonoidForList, _}
import cats.syntax.applicative._
import cats.syntax.functor._
import cats.{Applicative, Monad, MonadError, Monoid, Traverse}
import com.typesafe.config.{ConfigFactory, ConfigList, ConfigObject, ConfigValue, Config => TConfig}
import extruder.core.{EncoderT, Settings, Show}
import extruder.data.{StringWriter, Transform}
import extruder.typesafe.IntermediateTypes.{toConfig, Config, ConfigTypes}
import shapeless.Refute

import scala.collection.TraversableOnce

trait TypesafeConfigEncoderInstances {
  implicit val typesafeConfigMonoid: Monoid[Config] = catsKernelStdMonoidForList[ConfigTypes]

  implicit def typesafeConfigStringWriter[F[_]: Applicative]: StringWriter[F, Settings, Config] =
    new StringWriter[F, Settings, Config] {
      override def write(path: List[String], settings: Settings, value: String): F[Config] =
        List(ConfigTypes(settings.pathToString(path), value)).pure[F]
    }

  implicit def typesafeConfigFinalize[F[_]](
    implicit F: MonadError[F, Throwable]
  ): Transform[F, Settings, Config, TConfig] =
    Transform.inputByF[F, Settings, Config, TConfig] { inputData =>
      F.catchNonFatal(inputData.map(toConfig).foldLeft[TConfig](ConfigFactory.empty())((a, b) => b.withFallback(a)))
    }

  implicit def dataObjectEncoder[F[_]: Applicative]: EncoderT[F, Settings, ConfigObject, Config] =
    EncoderT.make[F, Settings, ConfigObject, Config] { (path, settings, value) =>
      List(ConfigTypes(settings.pathToString(path), value)).pure[F]
    }

  implicit def dataListEncoder[F[_]: Applicative]: EncoderT[F, Settings, ConfigList, Config] =
    EncoderT.make[F, Settings, ConfigList, Config] { (path, settings, value) =>
      List(ConfigTypes(settings.pathToString(path), value)).pure[F]
    }

  implicit def dataValueEncoder[F[_]: Applicative]: EncoderT[F, Settings, ConfigValue, Config] =
    EncoderT.make[F, Settings, ConfigValue, Config] { (path, settings, value) =>
      List(ConfigTypes(settings.pathToString(path), value)).pure[F]
    }

  implicit def traversableObjectEncoder[F[_]: Monad, A, FF[T] <: TraversableOnce[T]](
    implicit encoder: EncoderT[F, Settings, A, Config],
    refute: Refute[Show[A]]
  ): EncoderT[F, Settings, FF[A], Config] = EncoderT.make[F, Settings, FF[A], Config] { (path, settings, value) =>
    Traverse[List]
      .sequence[F, Config](value.toList.map(encoder.write(List.empty, settings, _)))
      .map(c => List(ConfigTypes.nested(settings.pathToString(path), c)))
  }

  implicit def traversableEncoder[F[_]: Applicative, A, FF[T] <: TraversableOnce[T]](
    implicit shows: Show[A]
  ): EncoderT[F, Settings, FF[A], Config] =
    EncoderT.make[F, Settings, FF[A], Config] { (path, settings, value) =>
      List(ConfigTypes(settings.pathToString(path), value.map(shows.show).toList)).pure[F]
    }
}
