package extruder.system

import cats.effect.IO
import extruder.core._

import scala.collection.JavaConverters._

trait EnvironmentDecoder[F[_], T] extends Decoder[F, T] {
  override type InputData = Map[String, String]
}

trait EnvironmentDecoders
    extends Decoders
    with PrimitiveDecoders
    with DerivedDecoders
    with Decode
    with DecodeFromDefaultSource
    with DecodeTypes {
  override type InputData = java.util.Map[String, String]
  override type DecodeData = Map[String, String]
  override type Dec[F[_], T] = EnvironmentDecoder[F, T]
  override type Hint = EnvironmentHints

  override protected def hasValue[F[_], E](
    path: List[String],
    data: Map[String, String]
  )(implicit hints: EnvironmentHints, AE: ExtruderApplicativeError[F, E]): IO[F[Boolean]] =
    lookupValue(path, data).map(AE.map(_)(_.isDefined))

  override protected def lookupValue[F[_], E](
    path: List[String],
    data: Map[String, String]
  )(implicit hints: Hint, AE: ExtruderApplicativeError[F, E]): IO[F[Option[String]]] =
    IO(AE.pure(data.get(hints.pathToString(path))))

  override protected def mkDecoder[F[_], T](
    f: (List[String], Option[T], Map[String, String]) => IO[F[T]]
  ): EnvironmentDecoder[F, T] =
    new EnvironmentDecoder[F, T] {
      override def read(path: List[String], default: Option[T], data: Map[String, String]): IO[F[T]] =
        f(path, default, data)
    }

  override protected def prepareInput[F[_], E](
    namespace: List[String],
    data: java.util.Map[String, String]
  )(implicit AE: ExtruderApplicativeError[F, E], util: Hint): IO[F[Map[String, String]]] =
    IO(AE.pure(data.asScala.toMap))

  override def loadInput: IO[java.util.Map[String, String]] = IO(System.getenv())
}

object EnvironmentDecoder extends EnvironmentDecoders

object EnvironmentSource extends EnvironmentDecoders

trait EnvironmentHints extends Hints

object EnvironmentHints extends HintsCompanion[EnvironmentHints] {
  override implicit val default: EnvironmentHints = new EnvironmentHints {
    override def pathToString(path: List[String]): String = path.mkString("_").toUpperCase
  }
}
