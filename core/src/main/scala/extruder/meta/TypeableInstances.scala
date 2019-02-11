package extruder.meta

import cats.data.{Chain, NonEmptyChain, NonEmptyVector}
import shapeless.Typeable
import shapeless.syntax.typeable._

import scala.reflect.ClassTag

trait TypeableInstances {
  implicit def chain[A](implicit mChain: ClassTag[Chain[_]], castT: Typeable[A]): Typeable[Chain[A]] =
    new Typeable[Chain[A]] {
      def cast(t: Any): Option[Chain[A]] =
        if (t == null) None
        else if (mChain.runtimeClass.isAssignableFrom(t.getClass)) {
          val cc = t.asInstanceOf[Chain[Any]]
          if (cc.forall(_.cast[A].isDefined)) Some(t.asInstanceOf[Chain[A]])
          else None
        } else None
      def describe = s"${mChain.runtimeClass.getSimpleName}[${castT.describe}]"
    }

  implicit def nonEmptyChain[A](
    implicit mNonEmptyChain: ClassTag[NonEmptyChain[_]],
    castT: Typeable[A]
  ): Typeable[NonEmptyChain[A]] =
    new Typeable[NonEmptyChain[A]] {
      def cast(t: Any): Option[NonEmptyChain[A]] =
        if (t == null) None
        else if (mNonEmptyChain.runtimeClass.isAssignableFrom(t.getClass)) {
          val cc = t.asInstanceOf[NonEmptyChain[Any]]
          if (cc.forall(_.cast[A].isDefined)) Some(t.asInstanceOf[NonEmptyChain[A]])
          else None
        } else None
      def describe = s"${mNonEmptyChain.runtimeClass.getSimpleName}[${castT.describe}]"
    }

  implicit def nonEmptyVector[A](
    implicit mNonEmptyVector: ClassTag[NonEmptyVector[_]],
    castT: Typeable[A]
  ): Typeable[NonEmptyVector[A]] =
    new Typeable[NonEmptyVector[A]] {
      def cast(t: Any): Option[NonEmptyVector[A]] =
        if (t == null) None
        else if (mNonEmptyVector.runtimeClass.isAssignableFrom(t.getClass)) {
          val cc = t.asInstanceOf[NonEmptyVector[Any]]
          if (cc.forall(_.cast[A].isDefined)) Some(t.asInstanceOf[NonEmptyVector[A]])
          else None
        } else None
      def describe = s"${mNonEmptyVector.runtimeClass.getSimpleName}[${castT.describe}]"
    }
}
