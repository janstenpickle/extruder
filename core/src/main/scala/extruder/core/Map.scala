package extruder.core

import cats.Monoid
import cats.effect.IO

trait BaseMapEncoders extends Encoders with PrimitiveEncoders with DerivedEncoders with EncodeTypes {
  override type EncodeData = Map[String, String]
  override type Enc[F[_], T] = MapEncoder[F, T]

  override protected val monoid: Monoid[Map[String, String]] = new Monoid[Map[String, String]] {
    override def empty: Map[String, String] = Map.empty
    override def combine(x: Map[String, String], y: Map[String, String]): Map[String, String] = x ++ y
  }

  override protected def writeValue[F[_], E](
    path: List[String],
    value: String
  )(implicit hints: Hint, AE: ExtruderApplicativeError[F, E]): IO[F[Map[String, String]]] =
    IO(AE.pure(Map(hints.pathToString(path) -> value)))

  override protected def mkEncoder[F[_], T](f: (List[String], T) => IO[F[Map[String, String]]]): MapEncoder[F, T] =
    new MapEncoder[F, T] {
      override def write(path: List[String], in: T): IO[F[Map[String, String]]] = f(path, in)
    }
}

trait BaseMapDecoders extends Decoders with PrimitiveDecoders with DerivedDecoders with DecodeTypes {
  override type DecodeData = Map[String, String]
  override type Dec[F[_], T] = MapDecoder[F, T]

  override protected def hasValue[F[_], E](
    path: List[String],
    data: Map[String, String]
  )(implicit hints: Hint, AE: ExtruderApplicativeError[F, E]): IO[F[Boolean]] =
    lookupValue(path, data).map(AE.map(_)(_.isDefined))

  override protected def lookupValue[F[_], E](
    path: List[String],
    data: Map[String, String]
  )(implicit hints: Hint, AE: ExtruderApplicativeError[F, E]): IO[F[Option[String]]] =
    IO(AE.pure(data.get(hints.pathToString(path))))

  override protected def mkDecoder[F[_], E, T](
    f: (List[String], Option[T], Map[String, String]) => IO[F[T]]
  ): MapDecoder[F, T] =
    new MapDecoder[F, T] {
      override def read(path: List[String], default: Option[T], data: Map[String, String]): IO[F[T]] =
        f(path, default, data)
    }
}

trait MapDecoder[F[_], T] extends Decoder[F, T] {
  override type DecodeData = Map[String, String]
}

trait MapDecoders extends BaseMapDecoders with Decode with MapDataSource {
  override def prepareInput[F[_], E](
    namespace: List[String],
    data: Map[String, String]
  )(implicit AE: ExtruderApplicativeError[F, E], util: Hint): IO[F[Map[String, String]]] =
    IO(AE.pure(data))
}

object MapDecoder extends MapDecoders

trait MapEncoder[F[_], T] extends Encoder[F, T, Map[String, String]]

trait MapEncoders extends BaseMapEncoders with Encode with MapDataSource {
  override protected def finalizeOutput[F[_], E](
    namespace: List[String],
    inter: Map[String, String]
  )(implicit AE: ExtruderApplicativeError[F, E], util: Hint): IO[F[Map[String, String]]] =
    IO(AE.pure(inter))
}

object MapEncoder extends MapEncoders

trait MapSource extends MapEncoders with MapDecoders

object MapSource extends MapSource

trait MapHints extends Hints

object MapHints extends HintsCompanion[MapHints] {
  override implicit val default: MapHints = new MapHints {
    override def pathToString(path: List[String]): String = path.mkString(".").toLowerCase
  }
}

trait MapDataSource extends DataSource {
  override type InputData = Map[String, String]
  override type OutputData = Map[String, String]
  override type Hint = MapHints
}
