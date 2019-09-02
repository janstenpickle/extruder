package extruder

import cats.laws.discipline.ExhaustiveCheck

object CoreTestInstances {
  implicit val intExhaustiveCheck: ExhaustiveCheck[Int] = ExhaustiveCheck.instance(0.to(999).toList)
}
