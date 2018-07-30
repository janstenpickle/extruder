package extruder.effect

import cats.{Eq, MonadError}
import cats.instances.string._

trait ThrowableEffectSuite[F[_]] { self: EffectSuite[F, Throwable] =>
  def FF: MonadError[F, Throwable]

  override def missingValue(message: String): Throwable = new NoSuchElementException(message)

  override def validationFailureValue(message: String): Throwable =
    new RuntimeException(message)

  override def validationExceptionValue(message: String, th: Throwable): Throwable =
    th

  override def errEq: Eq[Throwable] = Eq.by(_.toString)

  override def compareErrors[A](f: F[A], v1: Option[Throwable], v2: Option[Throwable])(implicit e: Eq[A]): Boolean = {
    val err: F[A] = (v1, v2) match {
      case (Some(e1), Some(_)) => FF.raiseError(e1)
      case (Some(e1), None) => FF.raiseError(e1)
      case (None, Some(e2)) => FF.raiseError(e2)
      case _ => f
    }
    feq[A].eqv(f, err)
  }
}
