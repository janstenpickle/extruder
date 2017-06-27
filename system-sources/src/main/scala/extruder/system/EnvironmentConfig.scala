package extruder.system

import cats.effect.IO
import extruder.core._

import scala.collection.JavaConverters._

trait EnvironmentDecoder[F[_], T] extends Decoder[F, T, Map[String, String]]

trait EnvironmentDecoders
    extends Decoders
    with PrimitiveDecoders
    with DerivedDecoders
    with Decode
    with DecodeFromDefaultConfig
    with DecodeTypes {
  override type InputConfig = java.util.Map[String, String]
  override type DecodeConfig = Map[String, String]
  override type Dec[F[_], T] = EnvironmentDecoder[F, T]
  override type Hint = EnvironmentHints

  override protected def hasValue[F[_], E](
    path: List[String],
    config: Map[String, String]
  )(implicit utils: EnvironmentHints, AE: ExtruderApplicativeError[F, E]): IO[F[Boolean]] =
    lookupValue(path, config).map(AE.map(_)(_.isDefined))

  override protected def lookupValue[F[_], E](
    path: List[String],
    config: Map[String, String]
  )(implicit utils: Hint, AE: ExtruderApplicativeError[F, E]): IO[F[Option[String]]] =
    IO(AE.pure(config.get(utils.pathToString(path))))

  override protected def mkDecoder[F[_], T](
    f: (List[String], Option[T], Map[String, String]) => IO[F[T]]
  ): EnvironmentDecoder[F, T] =
    new EnvironmentDecoder[F, T] {
      override def read(path: List[String], default: Option[T], config: Map[String, String]): IO[F[T]] =
        f(path, default, config)
    }

  override protected def prepareConfig[F[_], E](
    namespace: List[String],
    config: java.util.Map[String, String]
  )(implicit AE: ExtruderApplicativeError[F, E], util: Hint): IO[F[Map[String, String]]] =
    IO(AE.pure(config.asScala.toMap.map { case (k, v) => k.toUpperCase -> v }))

  override def loadConfig: IO[java.util.Map[String, String]] = IO(System.getenv())
}

object EnvironmentDecoder extends EnvironmentDecoders

object EnvironmentConfig extends EnvironmentDecoders

trait EnvironmentHints extends Hints

object EnvironmentHints extends HintsCompanion[EnvironmentHints] {
  override implicit val default: EnvironmentHints = new EnvironmentHints {
    override def pathToString(path: List[String]): String = path.mkString("_").toUpperCase
  }
}
