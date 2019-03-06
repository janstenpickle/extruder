package extruder.meta

import java.net.URL

import shapeless.Typeable

import scala.concurrent.duration.{Duration, FiniteDuration}

trait PrimitiveMetaInfoInstances {
  def primitive[A](t: String)(implicit tpe: Typeable[A]): Primitive[A] = new Primitive[A] {
    override val `type`: String = t
    override val typeable: Typeable[A] = tpe
  }

  implicit val char: Primitive[Char] = primitive[Char]("Char")
  implicit val string: Primitive[String] = primitive[String]("String")
  implicit val int: Primitive[Int] = primitive[Int]("Int")
  implicit val long: Primitive[Long] = primitive[Long]("Long")
  implicit val double: Primitive[Double] = primitive[Double]("Double")
  implicit val float: Primitive[Float] = primitive[Float]("Float")
  implicit val short: Primitive[Short] = primitive[Short]("Short")
  implicit val boolean: Primitive[Boolean] = primitive[Boolean]("Boolean")
  implicit val url: Primitive[URL] = primitive[URL]("URL")
  implicit val duration: Primitive[Duration] = primitive[Duration]("Duration")
  implicit val finiteDuration: Primitive[FiniteDuration] = primitive[FiniteDuration]("FiniteDuration")
}
