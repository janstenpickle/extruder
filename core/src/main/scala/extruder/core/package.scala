package extruder

import extruder.data.{ValidationError, ValidationErrors, ValidationException}

package object core {
  val TypeKey: String = "type"

  def errorsToThrowable(errs: ValidationErrors): Throwable = {
    val errorToThrowable: ValidationError => Throwable = {
      case e: ValidationException => e.exception
      case e: Any => e
    }

    errs.tail.foldLeft(errorToThrowable(errs.head)) { (acc, v) =>
      acc.addSuppressed(errorToThrowable(v))
      acc
    }
  }

  implicit object Dis1
  implicit object Dis2
  implicit object Dis3
}
