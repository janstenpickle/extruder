package extruder.core

import cats.Monoid
import cats.syntax.validated._

trait BaseMapEncoders extends Encoders[Map[String, String], MapEncoder]
                      with PrimitiveEncoders[Map[String, String], MapEncoder]
                      with DerivedEncoders[Map[String, String], MapEncoder]
                      with PeriodSeparatedPath {
  override protected val monoid: Monoid[Map[String, String]] = new Monoid[Map[String, String]] {
    override def empty: Map[String, String] = Map.empty
    override def combine(x: Map[String, String], y: Map[String, String]): Map[String, String] = x ++ y
  }

  override protected def writeValue(path: Seq[String], value: String): ConfigValidation[Map[String, String]] =
    Map(pathToString(path) -> value).validNel

  override protected def mkEncoder[T](f: (Seq[String], T) => ConfigValidation[Map[String, String]]): MapEncoder[T] =
    new MapEncoder[T] {
      override def write(path: Seq[String], in: T): ConfigValidation[Map[String, String]] = f(path, in)
    }
}

trait BaseMapDecoders extends Decoders[Map[String, String], MapDecoder]
                      with PrimitiveDecoders[Map[String, String], MapDecoder]
                      with DerivedDecoders[Map[String, String], MapDecoder]
                      with PeriodSeparatedPath {
  override protected def lookupValue(path: Seq[String], config: Map[String, String]): ConfigValidation[Option[String]] =
    config.get(pathToString(path)).validNel

  override protected def mkDecoder[T](f: (Seq[String], Option[T], Map[String, String]) => ConfigValidation[T]): MapDecoder[T] =
    new MapDecoder[T] {
      override def read(path: Seq[String], default: Option[T], config: Map[String, String]): ConfigValidation[T] = f(path, default, config)
    }
}

trait MapDecoder[T] extends Decoder[T, Map[String, String]]

trait MapDecoders extends BaseMapDecoders with Decode[Map[String, String], Map[String, String], MapDecoder] {
  override protected def prepareConfig(config: Map[String, String]): ConfigValidation[Map[String, String]] =
    config.map{ case (k, v) => (k.toLowerCase, v) }.validNel
}

object MapDecoder extends MapDecoders


trait MapEncoder[T] extends Encoder[T, Map[String, String]]

trait MapEncoders extends BaseMapEncoders with Encode[Map[String, String], Map[String, String], MapEncoder] {
  override protected def finalizeConfig(inter: Map[String, String]): ConfigValidation[Map[String, String]] = inter.validNel
}

object MapEncoder extends MapEncoders

trait MapConfig extends MapEncoders with MapDecoders

object MapConfig extends MapConfig
