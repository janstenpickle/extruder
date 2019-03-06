package extruder.core

import shapeless.{LabelledGeneric, Refute}

trait CombinedRefute[A] {}

object CombinedRefute {
  def apply[A](implicit combinedRefute: CombinedRefute[A]): CombinedRefute[A] = combinedRefute

  implicit def parser[A](implicit lg: LabelledGeneric[A], p: Refute[Parser[A]]): CombinedRefute[A] =
    new CombinedRefute[A] {}
  implicit def multiParser[F[_], A](implicit lg: LabelledGeneric[A], p: Refute[MultiParser[F, A]]): CombinedRefute[A] =
    new CombinedRefute[A] {}
}
