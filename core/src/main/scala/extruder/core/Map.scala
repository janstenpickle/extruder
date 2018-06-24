package extruder.core

import cats.Monoid
import extruder.effect.ExtruderMonadError

trait BaseMapEncoders
    extends Encoders
    with PrimitiveEncoders
    with StringMapEncoders
    with DerivedEncoders
    with EncodeTypes {
  override type EncodeData = Map[String, String]
  override type Enc[F[_], T] = MapEncoder[F, T]
  override type Sett = Settings

  override protected val monoid: Monoid[Map[String, String]] = new Monoid[Map[String, String]] {
    override def empty: Map[String, String] = Map.empty
    override def combine(x: Map[String, String], y: Map[String, String]): Map[String, String] = x ++ y
  }

  override protected def writeValue[F[_]](path: List[String], settings: Sett, value: String)(
    implicit F: Eff[F]
  ): F[Map[String, String]] =
    F.pure(Map(settings.pathToString(path) -> value))

  override protected def mkEncoder[F[_], T](f: (List[String], Sett, T) => F[Map[String, String]]): MapEncoder[F, T] =
    new MapEncoder[F, T] {
      override def write(path: List[String], settings: Sett, in: T): F[Map[String, String]] = f(path, settings, in)
    }
}

trait BaseMapDecoders
    extends Decoders
    with PrimitiveDecoders
    with StringMapDecoders
    with DerivedDecoders
    with DecodeTypes {
  override type DecodeData = Map[String, String]
  override type Dec[F[_], T] = MapDecoder[F, T]
  override type Sett = Settings

  override protected def hasValue[F[_]](path: List[String], settings: Sett, data: Map[String, String])(
    implicit F: Eff[F]
  ): F[Boolean] =
    F.map(lookupValue(path, settings, data))(_.isDefined)

  override protected def lookupValue[F[_]](path: List[String], settings: Sett, data: Map[String, String])(
    implicit F: Eff[F]
  ): F[Option[String]] =
    F.pure(data.get(settings.pathToString(path)))

  override protected def prune[F[_]](path: List[String], settings: Sett, data: Map[String, String])(
    implicit F: Eff[F]
  ): F[Option[(List[String], Map[String, String])]] = {
    val basePath = s"${settings.pathToString(path)}."
    val pruned = data.filterKeys(_.startsWith(basePath))
    F.pure(
      if (pruned.isEmpty) None
      else Some(pruned.keys.map(_.replace(basePath, "")).toList -> pruned)
    )
  }

  override protected def mkDecoder[F[_], T](
    f: (List[String], Sett, Option[T], Map[String, String]) => F[T]
  ): MapDecoder[F, T] =
    new MapDecoder[F, T] {
      override def read(path: List[String], settings: Sett, default: Option[T], data: Map[String, String]): F[T] =
        f(path, settings, default, data)
    }
}

trait MapDecoder[F[_], T] extends Decoder[F, Settings, T, Map[String, String]]

trait MapDecoders extends BaseMapDecoders with Decode with MapDataSource {
  override protected def prepareInput[F[_]](namespace: List[String], settings: Sett, data: Map[String, String])(
    implicit F: Eff[F]
  ): F[Map[String, String]] =
    F.pure(data)
}

object MapDecoder extends MapDecoders

trait MapEncoder[F[_], T] extends Encoder[F, Settings, T, Map[String, String]]

trait MapEncoders extends BaseMapEncoders with Encode with MapDataSource {
  override protected def finalizeOutput[F[_]](namespace: List[String], settings: Sett, inter: Map[String, String])(
    implicit F: Eff[F]
  ): F[Map[String, String]] =
    F.pure(inter)
}

object MapEncoder extends MapEncoders

trait MapSource extends MapEncoders with MapDecoders

object MapSource extends MapSource

trait MapDataSource extends DataSource {
  override type InputData = Map[String, String]
  override type OutputData = Map[String, String]
  override type Eff[F[_]] = ExtruderMonadError[F]
  override type Sett = Settings

  override val defaultSettings: Sett = new Settings {
    override def pathToString(path: List[String]): String = path.mkString(".").toLowerCase
  }
}
