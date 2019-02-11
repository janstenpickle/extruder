package extruder.data

import cats.implicits._
import cats.laws.discipline.MonadErrorTests
import extruder.data.ValidationCatsInstances._
import org.scalatest.FunSuite
import org.typelevel.discipline.scalatest.Discipline

class ValidationSuite extends FunSuite with Discipline {
  checkAll("Validation", MonadErrorTests[Validation, Throwable].monadError[Int, Int, Int])
}
