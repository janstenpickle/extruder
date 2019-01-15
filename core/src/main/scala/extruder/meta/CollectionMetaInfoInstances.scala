package extruder.meta

import cats.data.{Chain, NonEmptyChain, NonEmptyList, NonEmptyVector}
import shapeless.Typeable

import scala.collection.generic.CanBuildFrom
import scala.reflect.runtime.universe.TypeTag

trait CollectionMetaInfoInstances {
  implicit def traversable[F[T] <: TraversableOnce[T], A](
    implicit ev: MetaInfo[A],
    cbf: CanBuildFrom[F[A], A, F[A]],
    tag: TypeTag[F[A]],
    tpe: Typeable[F[A]]
  ): Collection[F, A] = new Collection[F, A] {
    override val elements: MetaInfo[A] = ev
    override val collectionType: String = tag.tpe.typeSymbol.name.toString
    override val typeable: Typeable[F[A]] = tpe
  }

  implicit def nonEmptyListMetaInfo[A](
    implicit ev: MetaInfo[A],
    tpe: Typeable[NonEmptyList[A]]
  ): Collection[NonEmptyList, A] =
    new Collection[NonEmptyList, A] {
      override val elements: MetaInfo[A] = ev
      override val collectionType: String = "NonEmptyList"
      override val `type`: String = s"$collectionType[${elements.`type`}]"
      override val typeable: Typeable[NonEmptyList[A]] = tpe
    }

  implicit def nonEmptyVectorMetaInfo[A](
    implicit ev: MetaInfo[A],
    tpe: Typeable[NonEmptyVector[A]]
  ): Collection[NonEmptyVector, A] =
    new Collection[NonEmptyVector, A] {
      override val elements: MetaInfo[A] = ev
      override val collectionType: String = "NonEmptyVector"
      override val `type`: String = s"$collectionType[${elements.`type`}]"
      override val typeable: Typeable[NonEmptyVector[A]] = tpe
    }

  implicit def chainMetaInfo[A](implicit ev: MetaInfo[A], tpe: Typeable[Chain[A]]): Collection[Chain, A] =
    new Collection[Chain, A] {
      override val elements: MetaInfo[A] = ev
      override val collectionType: String = "Chain"
      override val typeable: Typeable[Chain[A]] = tpe
    }

  implicit def nonEmptyChainMetaInfo[A](
    implicit ev: MetaInfo[A],
    tpe: Typeable[NonEmptyChain[A]]
  ): Collection[NonEmptyChain, A] =
    new Collection[NonEmptyChain, A] {
      override val elements: MetaInfo[A] = ev
      override val collectionType: String = "NonEmptyChain"
      override val typeable: Typeable[NonEmptyChain[A]] = tpe
    }
}
