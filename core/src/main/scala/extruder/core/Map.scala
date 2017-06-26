package extruder.core

import cats.Monoid
import cats.effect.IO

trait BaseMapEncoders extends Encoders
                      with PrimitiveEncoders
                      with DerivedEncoders
                      with EncodeTypes {
  override type EncodeConfig = Map[String, String]
  override type Enc[F[_], T] = MapEncoder[F, T]

  override protected val monoid: Monoid[Map[String, String]] = new Monoid[Map[String, String]] {
    override def empty: Map[String, String] = Map.empty
    override def combine(x: Map[String, String], y: Map[String, String]): Map[String, String] = x ++ y
  }

  override protected def writeValue[F[_], E](path: Seq[String], value: String)(implicit utils: Hint, AE: ExtruderApplicativeError[F, E]): IO[F[Map[String, String]]] =
    IO(AE.pure(Map(utils.pathToString(path) -> value)))

  override protected def mkEncoder[F[_], T](f: (Seq[String], T) => IO[F[Map[String, String]]]): MapEncoder[F, T] =
    new MapEncoder[F, T] {
      override def write(path: Seq[String], in: T): IO[F[Map[String, String]]] = f(path, in)
    }
}

trait BaseMapDecoders extends Decoders
                      with PrimitiveDecoders
                      with DerivedDecoders
                      with DecodeTypes {
  override type DecodeConfig = Map[String, String]
  override type Dec[F[_], T] = MapDecoder[F, T]

  override protected def hasValue[F[_], E](path: Seq[String], config: Map[String, String])(implicit utils: Hint, AE: ExtruderApplicativeError[F, E]): IO[F[Boolean]] =
    lookupValue(path, config).map(AE.map(_)(_.isDefined))

  override protected def lookupValue[F[_], E](path: Seq[String], config: Map[String, String])(implicit utils: Hint, AE: ExtruderApplicativeError[F, E]): IO[F[Option[String]]] =
    IO(AE.pure(config.get(utils.pathToString(path))))

  override protected def mkDecoder[F[_], T](f: (Seq[String], Option[T], Map[String, String]) => IO[F[T]]): MapDecoder[F, T] =
    new MapDecoder[F, T] {
      override def read(path: Seq[String], default: Option[T], config: Map[String, String]): IO[F[T]] = f(path, default, config)
    }
}

trait MapDecoder[F[_], T] extends Decoder[F, T, Map[String, String]]

trait MapDecoders extends BaseMapDecoders with Decode with MapConfigSource {
  override protected def prepareConfig[F[_], E ](namespace: Seq[String], config: Map[String, String])
                                                (implicit AE: ExtruderApplicativeError[F, E], util: Hint): IO[F[Map[String, String]]] =
    IO(AE.pure(config.map{ case (k, v) => (k.toLowerCase, v) }))
}

object MapDecoder extends MapDecoders

trait MapEncoder[F[_], T] extends Encoder[F, T, Map[String, String]]

trait MapEncoders extends BaseMapEncoders with Encode with MapConfigSource {
  override protected def finalizeConfig[F[_], E](namespace: Seq[String], inter: Map[String, String])
                                                (implicit AE: ExtruderApplicativeError[F, E], util: Hint): IO[F[Map[String, String]]] =
    IO(AE.pure(inter))
}

object MapEncoder extends MapEncoders

trait MapConfig extends MapEncoders with MapDecoders

object MapConfig extends MapConfig

trait MapHints extends Hints

object MapHints extends HintsCompanion[MapHints] {
  override implicit val default: MapHints = new MapHints {
    override def pathToString(path: Seq[String]): String = path.mkString(".").toLowerCase
  }
}

trait MapConfigSource extends ConfigSource {
  override type InputConfig = Map[String, String]
  override type OutputConfig = Map[String, String]
  override type Hint = MapHints
}