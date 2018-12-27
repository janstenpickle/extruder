package extruder.meta

import shapeless.labelled.FieldType
import shapeless.{Witness, _}

import scala.reflect.runtime.universe.TypeTag

trait UnionMetaInfoInstances {
  implicit val cnilMetaInfo: DerivedUnion[CNil] = new DerivedUnion[CNil] {
    override def options: Set[BaseMetaInfo] = Set.empty
  }

  private[meta] trait DerivedUnion[A] {
    def options: Set[BaseMetaInfo]
  }

  implicit def derivedUnion[A](implicit ev: MetaInfo[A]): DerivedUnion[A] = new DerivedUnion[A] {
    override def options: Set[BaseMetaInfo] = Set(ev)
  }

  implicit def cconsMetaInfo[K <: Symbol, H, T <: Coproduct](
    implicit key: Witness.Aux[K],
    head: Lazy[DerivedUnion[H]],
    tail: Lazy[DerivedUnion[T]]
  ): DerivedUnion[FieldType[K, H] :+: T] = {
    new DerivedUnion[FieldType[K, H] :+: T] {
      override def options: Set[BaseMetaInfo] = head.value.options ++ tail.value.options
    }
  }

  implicit def unionMetaInfo[A, Repr <: Coproduct](
    implicit gen: LabelledGeneric.Aux[A, Repr],
    tag: TypeTag[A],
    tpe: Typeable[A],
    ev: Lazy[DerivedUnion[Repr]],
//    refutePrimitive: Refute[Primitive[A]],
    neOpt: A <:!< Option[_],
    neCol: A <:!< TraversableOnce[_]
  ): Union[A] = new Union[A] {
    override val `type`: String = tag.tpe.typeSymbol.name.toString
    override val options: Set[BaseMetaInfo] = ev.value.options
    override def typeable: Typeable[A] = tpe
  }
}
