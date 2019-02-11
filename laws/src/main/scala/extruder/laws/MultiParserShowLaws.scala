package extruder.laws

import cats.Applicative
import cats.data.{OptionT, ValidatedNel}
import cats.data.Validated.Valid
import cats.laws._
import cats.kernel.laws.IsEq
import extruder.core.{MultiParser, MultiShow}

trait MultiParserShowLaws[F[_], A] {
  implicit def F: Applicative[F]
  def multiParser: MultiParser[F, A]
  def multiShow: MultiShow[A]

  def showParse(a: A): IsEq[OptionT[F, ValidatedNel[String, A]]] = {
    val data = multiShow.show(a)

    multiParser.parse(li => OptionT.fromOption(data.get(li))) <-> OptionT.pure(Valid(a))
  }

  def parseEmpty(dummy: Boolean): IsEq[OptionT[F, ValidatedNel[String, A]]] =
    multiParser.parse(_ => OptionT.none) <-> OptionT.none
}

object MultiParserShowLaws {
  def apply[F[_]: Applicative, A](implicit parser: MultiParser[F, A], show: MultiShow[A]): MultiParserShowLaws[F, A] =
    new MultiParserShowLaws[F, A] {
      override def F: Applicative[F] = Applicative[F]
      override def multiParser: MultiParser[F, A] = parser
      override def multiShow: MultiShow[A] = show
    }
}
