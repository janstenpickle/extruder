package extruder.meta

import extruder.core.Show
import extruder.meta.Product.MetaInfoWithDefault
import shapeless.labelled.FieldType
import shapeless.{Witness, _}

import scala.reflect.runtime.universe.TypeTag

trait ProductMetaInfoInstances {
  private[meta] trait DerivedMeta[A, Repr <: HList, DefaultRepr <: HList] {
    def values(default: DefaultRepr): Map[String, MetaInfoWithDefault]
  }
  implicit def hnilMetaInfo[A]: DerivedMeta[A, HNil, HNil] = new DerivedMeta[A, HNil, HNil] {
    override def values(default: HNil): Map[String, MetaInfoWithDefault] = Map.empty
  }

  implicit def hConsDerivedMetaInfo[A, K <: Symbol, V, TailRepr <: HList, DefaultTailRepr <: HList](
    implicit key: Witness.Aux[K],
    ev: Lazy[MetaInfo[V]],
    tailEv: Lazy[DerivedMeta[A, TailRepr, DefaultTailRepr]],
    show: Show[V] = Show.by[String, V](_.toString)
  ): DerivedMeta[A, FieldType[K, V] :: TailRepr, Option[V] :: DefaultTailRepr] = {
    val fieldName = key.value.name

    new DerivedMeta[A, FieldType[K, V] :: TailRepr, Option[V] :: DefaultTailRepr] {
      override def values(default: Option[V] :: DefaultTailRepr): Map[String, MetaInfoWithDefault] =
        tailEv.value.values(default.tail) + (fieldName -> MetaInfoWithDefault(ev.value, default.head.map(show.show)))
    }
  }

  implicit def productMetaInfo[A, GenRepr <: HList, DefaultOptsRepr <: HList](
    implicit gen: LabelledGeneric.Aux[A, GenRepr],
    defaults: Default.AsOptions.Aux[A, DefaultOptsRepr],
    tag: TypeTag[A],
    tpe: Typeable[A],
    ev: Lazy[DerivedMeta[A, GenRepr, DefaultOptsRepr]],
    lp: LowPriority
  ): Product[A] =
    new Product[A] {
      override val `type`: String = tag.tpe.typeSymbol.name.toString
      override val children: Map[String, MetaInfoWithDefault] = ev.value.values(defaults.apply())
      override val typeable: Typeable[A] = tpe
    }
}
