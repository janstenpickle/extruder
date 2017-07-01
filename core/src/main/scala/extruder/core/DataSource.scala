package extruder.core

trait DataSource {
  type InputData
  type OutputData
  type Hint <: Hints

  def errorsToThrowable(errs: ValidationErrors): Throwable = {
    val errorToThrowable: ValidationError => Throwable = {
      case e: ValidationException => e.exception
      case e: Any => new RuntimeException(e.message)
    }

    errs.tail.foldLeft(errorToThrowable(errs.head)) { (acc, v) =>
      acc.addSuppressed(errorToThrowable(v))
      acc
    }
  }
}
