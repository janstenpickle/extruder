package extruder

import cats.syntax.either._
import eu.timepit.refined.api.{RefType, Validate}
import extruder.core._

import scala.reflect.runtime.universe.WeakTypeTag

package object refined {
  implicit def refinedParser[T, F[_, _], P](implicit parser: Parser[T],
                                               refType: RefType[F],
                                               validate: Validate[T, P],
                                               typeTag: WeakTypeTag[F[T, P]]): Parser[F[T, P]] = Parser(value =>
    parser.parse(value).flatMap(parsed =>
      refType.refine[P](parsed)
    )
  )

  implicit def refinedShow[T, F[_, _], P](implicit shows: Show[T],
                                          refType: RefType[F],
                                          validate: Validate[T, P],
                                          typeTag: WeakTypeTag[F[T, P]]): Show[F[T, P]] =
    new Show(value => shows.show(refType.unwrap(value)))
}
