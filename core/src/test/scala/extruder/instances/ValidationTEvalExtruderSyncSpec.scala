package extruder.instances

import cats.{Eq, Eval}
import extruder.core.Validation
import extruder.data.ValidationT
import extruder.effect.{ExtruderSync, ExtruderSyncSpec}

class ValidationTEvalExtruderSyncSpec
    extends ExtruderSyncSpec[ValidationT[Eval, ?]]()(ExtruderSync.validationTEvalSync) {
  implicit def validationEq[A]: Eq[Validation[A]] = Eq.fromUniversalEquals

  override implicit def feq[A](implicit eq: Eq[A]): Eq[ValidationT[Eval, A]] = Eq.by(_.value.value)
}
