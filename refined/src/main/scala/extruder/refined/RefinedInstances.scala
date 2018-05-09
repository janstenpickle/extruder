package extruder.refined

import eu.timepit.refined.api.{RefType, Validate}
import extruder.core._

import scala.reflect.runtime.universe.WeakTypeTag

trait RefinedInstances {
  implicit def refinedParser[T, F[_, _], P](
    implicit parser: Parser[T],
    refType: RefType[F],
    validate: Validate[T, P],
    typeTag: WeakTypeTag[F[T, P]]
  ): Parser[F[T, P]] =
    parser.flatMapResult(refType.refine[P](_))

  implicit def refinedShow[T, F[_, _], P](
    implicit shows: Show[T],
    refType: RefType[F],
    validate: Validate[T, P],
    typeTag: WeakTypeTag[F[T, P]]
  ): Show[F[T, P]] =
    Show.by(refType.unwrap)
}
