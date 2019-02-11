package extruder.tests

import cats.Eq
import cats.data.{OptionT, ValidatedNel}
import cats.instances.all._
import extruder.data.Validation
import extruder.laws.MultiParserShowTests
import org.scalatest.FunSuite
import org.typelevel.discipline.scalatest.Discipline

class MultiParserShowSuite extends FunSuite with Discipline {
  implicit val eq: Eq[OptionT[Validation, ValidatedNel[String, (Int, Int)]]] =
    OptionT.catsDataEqForOptionT[Validation, ValidatedNel[String, (Int, Int)]]

  checkAll("Int Tuple", MultiParserShowTests[Validation, (Int, Int)].parserShow)
}
