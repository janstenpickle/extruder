package extruder.instances

import cats.data.EitherT
import cats.{Applicative, Bimonad, Eq, Eval}
import extruder.core.ValidationErrors
import extruder.effect.{ExtruderSync, ExtruderSyncSpec, ValidationTEffectSpec}

class ValidationTEvalExtruderSyncSpec
    extends ExtruderSyncSpec[EitherT[Eval, ValidationErrors, ?], ValidationErrors]()(ExtruderSync.eitherTEvalSync)
    with ValidationTEffectSpec[Eval] {

  override implicit def feq[A](implicit eq: Eq[A]): Eq[EitherT[Eval, ValidationErrors, A]] =
    Eq.instance { (x, y) =>
      validationEq.eqv(x.value.value, y.value.value)
    }

  override def getError[A](fa: EitherT[Eval, ValidationErrors, A]): ValidationErrors = fa.value.value.left.get

  override implicit def FF: Applicative[Eval] = Bimonad[Eval] // use the bimonad instance for stack safety
}
