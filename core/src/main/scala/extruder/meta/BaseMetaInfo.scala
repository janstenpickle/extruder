package extruder.meta

import cats.Eq
import shapeless.Typeable

sealed trait BaseMetaInfo {
  def `type`: String
  def typeable: Typeable[_]
  def isA[A](implicit t: Typeable[A], eq: Eq[Typeable[_]] = Eq.fromUniversalEquals): Boolean = eq.eqv(typeable, t)
}

trait MetaInfo[A] extends BaseMetaInfo
object MetaInfo
    extends PrimitiveMetaInfoInstances
    with CollectionMetaInfoInstances
    with ProductMetaInfoInstances
    with UnionMetaInfoInstances {
  implicit def optionalMetaInfo[A: Typeable](implicit ev: MetaInfo[A]): MetaInfo[Option[A]] = new Optional[A] {
    override def value: MetaInfo[A] = ev
    override def typeable: Typeable[Option[A]] = Typeable[Option[A]]
  }
  implicit def map[A: Typeable, B: Typeable](implicit ev0: MetaInfo[A], ev1: MetaInfo[B]): MapInfo[A, B] =
    new MapInfo[A, B] {
      override def keys: MetaInfo[A] = ev0
      override def values: MetaInfo[B] = ev1
      override def typeable: Typeable[Map[A, B]] = Typeable[Map[A, B]]
    }

  implicit def eitherMetaInfo[A: Typeable, B: Typeable](
    implicit ev0: MetaInfo[A],
    ev1: MetaInfo[A]
  ): Union[Either[A, B]] =
    new Union[Either[A, B]] {
      override def options: Set[BaseMetaInfo] = Set(ev0, ev1)
      override val `type`: String = s"Either[${ev0.`type`}, ${ev1.`type`}]"
      override def typeable: Typeable[Either[A, B]] = Typeable[Either[A, B]]
    }
}

trait Primitive[A] extends MetaInfo[A]

trait Collection[F[_], A] extends MetaInfo[F[A]] {
  override def `type`: String = s"$collectionType[${elements.`type`}]"
  def collectionType: String
  def elements: MetaInfo[A]
}

trait Optional[A] extends MetaInfo[Option[A]] {
  override def `type`: String = s"Option[${value.`type`}]"
  def value: MetaInfo[A]
}

trait MapInfo[A, B] extends MetaInfo[Map[A, B]] {
  override def `type`: String = s"Map[${keys.`type`}, ${values.`type`}]"
  def keys: MetaInfo[A]
  def values: MetaInfo[B]
}

trait Product[A] extends MetaInfo[A] { outer =>
  def children: Map[String, Product.MetaInfoWithDefault]
}

object Product {
  case class MetaInfoWithDefault(metaInfo: BaseMetaInfo, default: Option[String])
}

trait Union[A] extends MetaInfo[A] {
  def options: Set[BaseMetaInfo]
}
