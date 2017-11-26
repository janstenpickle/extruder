package extruder.instances

import cats.{Applicative, Bimonad, Eq, Eval}
import extruder.core.{Validation, ValidationErrors}
import extruder.data.ValidationT
import extruder.effect.{ExtruderSync, ExtruderSyncSpec, ValidationTEffectSpec}

class ValidationTEvalExtruderSyncSpec
    extends ExtruderSyncSpec[ValidationT[Eval, ?], ValidationErrors]()(ExtruderSync.validationTEvalSync)
    with ValidationTEffectSpec[Eval] {

  override implicit def feq[A](implicit eq: Eq[A]): Eq[ValidationT[Eval, A]] =
    Eq.instance { (x, y) =>
      validationEq.eqv(x.value.value, y.value.value)
    }

  override def getError[A](fa: ValidationT[Eval, A]): ValidationErrors = fa.value.value.toEither.left.get

  override implicit def FF: Applicative[Eval] = Bimonad[Eval] // use the bimonad instance for stack safety
}
