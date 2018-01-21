package extruder.typesafe

import cats.Functor
import com.typesafe.config.{ConfigFactory, ConfigList, ConfigObject, ConfigValue, Config => TConfig}
import shapeless._

import scala.collection.JavaConverters._

object IntermediateTypes {

  type ConfigRepr[A] =
    (String, String) :+: (String, ConfigValue) :+: (String, ConfigList) :+: (String, ConfigObject) :+: (
      String,
      List[String]
    ) :+: (String, List[List[A]]) :+: CNil

  type ConfigTypes = Fix[ConfigRepr]
  type Config = List[ConfigTypes]

  case class Fix[F[_]](unfix: F[Fix[F]])

  def cata[A, F[_]](f: F[A] => A)(t: Fix[F])(implicit F: Functor[F]): A =
    f(F.map(t.unfix)(cata[A, F](f)(_)))

  object ConfigTypes {
    def apply(k: String, s: String): ConfigTypes =
      Fix(Coproduct[ConfigRepr[Fix[ConfigRepr]]](k -> s))
    def apply(k: String, cv: ConfigValue): ConfigTypes =
      Fix(Coproduct[ConfigRepr[Fix[ConfigRepr]]](k -> cv))
    def apply(k: String, cl: ConfigList): ConfigTypes =
      Fix(Coproduct[ConfigRepr[Fix[ConfigRepr]]](k -> cl))
    def apply(k: String, co: ConfigObject): ConfigTypes =
      Fix(Coproduct[ConfigRepr[Fix[ConfigRepr]]](k -> co))
    def apply(k: String, sl: List[String]): ConfigTypes =
      Fix(Coproduct[ConfigRepr[Fix[ConfigRepr]]](k -> sl))
    def nested(k: String, l: List[List[ConfigTypes]]): ConfigTypes =
      Fix(Coproduct[ConfigRepr[Fix[ConfigRepr]]](k -> l))
  }

  implicit val functor: Functor[ConfigRepr] = new Functor[ConfigRepr] {
    override def map[A, B](fa: ConfigRepr[A])(f: A => B): ConfigRepr[B] = fa match {
      case Inl(s) => Inl(s)
      case Inr(Inl(cv)) => Inr(Inl(cv))
      case Inr(Inr(Inl(cl))) => Inr(Inr(Inl(cl)))
      case Inr(Inr(Inr(Inl(co)))) => Inr(Inr(Inr(Inl(co))))
      case Inr(Inr(Inr(Inr(Inl(sl))))) => Inr(Inr(Inr(Inr(Inl(sl)))))
      case Inr(Inr(Inr(Inr(Inr(Inl((k, l))))))) => Inr(Inr(Inr(Inr(Inr(Inl(k -> l.map(_.map(f))))))))
      case Inr(Inr(Inr(Inr(Inr(Inr(_)))))) => throw new RuntimeException("Impossible!")
    }
  }

  def toConfig(fix: Fix[ConfigRepr]): TConfig =
    cata[TConfig, ConfigRepr] {
      case Inl(s) => ConfigFactory.parseMap(Map(s).asJava)
      case Inr(Inl(cv)) => ConfigFactory.parseMap(Map(cv).asJava)
      case Inr(Inr(Inl(cl))) => ConfigFactory.parseMap(Map(cl).asJava)
      case Inr(Inr(Inr(Inl(co)))) => ConfigFactory.parseMap(Map(co).asJava)
      case Inr(Inr(Inr(Inr(Inl((k, sl)))))) => ConfigFactory.parseMap(Map((k, sl.asJava)).asJava)
      case Inr(Inr(Inr(Inr(Inr(Inl((k, l))))))) =>
        ConfigFactory.parseMap(
          Map(k -> l.map(_.foldLeft(ConfigFactory.empty())((a, b) => b.withFallback(a)).root()).asJava).asJava
        )
      case Inr(Inr(Inr(Inr(Inr(Inr(_)))))) => throw new RuntimeException("Impossible!")
    }(fix)
}
