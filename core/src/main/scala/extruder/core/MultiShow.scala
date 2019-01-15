package extruder.core

import extruder.instances.MultiShowInstances

/**
  * Converts value `A` into a `Map[String, String]` where each element in the map represents encoded
  * parameters of `A`.
  *
  * Implicit `EncoderT` instances will be constructed from every `MultiShow` instance where
  * a `StringWriter` instance also exists.
  *
  * @tparam A input value to be converted into a `Map[String, String]`
  */
trait MultiShow[A] { outer =>
  def show(a: A): Map[List[String], String]
  def contramap[B](f: B => A): MultiShow[B] = new MultiShow[B] {
    override def show(a: B): Map[List[String], String] = outer.show(f(a))
  }
}

object MultiShow extends MultiShowInstances {
  def apply[T](implicit multiShow: MultiShow[T]): MultiShow[T] = multiShow
  def by[T, V](f: V => T)(implicit ev: MultiShow[T]): MultiShow[V] =
    ev.contramap(f)

  implicit def tuple2MultiShow[A: Show, B: Show]: MultiShow[(A, B)] = new MultiShow[(A, B)] {
    def show(a: (A, B)): Map[List[String], String] =
      Map(List("_1") -> Show[A].show(a._1), List("_2") -> Show[B].show(a._2))
  }
}
