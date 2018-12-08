package extruder.map

import cats.syntax.applicative._
import cats.{Applicative, Monoid}
import extruder.core.Settings
import extruder.data.StringWriter

trait MapEncoderInstances {
  implicit def mapEncoderStringWriter[F[_]: Applicative]: StringWriter[F, Settings, Map[String, String]] =
    new StringWriter[F, Settings, Map[String, String]] {
      override def write(path: List[String], settings: Settings, value: String): F[Map[String, String]] =
        Map(settings.pathToString(path) -> value).pure[F]
    }

  implicit val mapMonoid: Monoid[Map[String, String]] = new Monoid[Map[String, String]] {
    override def empty: Map[String, String] = Map.empty
    override def combine(x: Map[String, String], y: Map[String, String]): Map[String, String] = x ++ y
  }
}
