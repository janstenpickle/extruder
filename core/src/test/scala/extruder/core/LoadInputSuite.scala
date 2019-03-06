package extruder.core

import cats.Id
import cats.instances.map._
import cats.instances.string._
import extruder.data.{Validation, ValidationErrors}
import org.scalatest.{EitherValues, FunSuite}

class LoadInputSuite extends FunSuite with EitherValues {
  import LoadInputSuite._

  test("Can load combined") {
    val ret = LoadInput[Id, (Map[String, String], Map[String, String])].load

    assert(ret._1 === input)
    assert(ret._2 === input)
  }

  test("Can load combined, fallback to empty") {
    val ret = LoadInput[Validation, (Map[String, String], Map[String, String])].load

    assert(ret.right.value._1 === input)
    assert(ret.right.value._2 === input)
  }

  test("Can load combined, fallback to empty string") {
    val ret = LoadInput[Validation, (String, String)].load

    assert(ret.right.value._1 === "")
    assert(ret.right.value._2 === "")
  }
}

object LoadInputSuite {
  val input = Map("a" -> "b")

  implicit val loadInput: LoadInput[Id, Map[String, String]] = new LoadInput[Id, Map[String, String]] {
    override def load: Id[Map[String, String]] = input
  }

  implicit val emptyInput: LoadInput[Validation, Map[String, String]] =
    new LoadInput[Validation, Map[String, String]] {
      override def load: Validation[Map[String, String]] = Validation(Right(input))
    }

  implicit val errorInput: LoadInput[Validation, String] =
    new LoadInput[Validation, String] {
      override def load: Validation[String] = Validation(Left(ValidationErrors.failure("error")))
    }
}
