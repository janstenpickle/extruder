package extruder.laws

import cats.kernel.laws.IsEq
import cats.laws._
import cats.{Applicative, Monoid}
import extruder.core.{DecoderT, ExtruderErrors, Settings}
import extruder.data.PathElement
import org.scalacheck.{Arbitrary, Gen}

trait DecoderLaws[F[_], S <: Settings, E, D, O] {
  implicit def F: Applicative[F]
  def settings: S
  def outMonoid: Monoid[O]
  def errors: ExtruderErrors[F]

  implicit val nonEmptyStringArb: Arbitrary[String] = Arbitrary(Gen.alphaNumStr.suchThat(_.nonEmpty))
  implicit val pathElemsArb: Arbitrary[List[PathElement]] = Arbitrary(
    Gen.nonEmptyListOf(nonEmptyStringArb.arbitrary.map(PathElement.Standard))
  )

  def decodeEmpty[A](path: List[PathElement])(implicit decoder: DecoderT[F, S, A, O]): IsEq[F[A]] =
    decoder.read(path, settings, None, outMonoid.empty) <-> errors.missing(
      s"Could not find value at '${settings.pathElementListToString(path)}' and no default available"
    )

  def decodeDefault[A](a: A, path: List[PathElement])(implicit decoder: DecoderT[F, S, A, O]): IsEq[F[A]] =
    decoder.read(path, settings, Some(a), outMonoid.empty) <-> F.pure(a)
}

object DecoderLaws {
  def apply[F[_]: Applicative: ExtruderErrors, S <: Settings, E, D, O: Monoid](s: S): DecoderLaws[F, S, E, D, O] =
    new DecoderLaws[F, S, E, D, O] {
      override def F: Applicative[F] = Applicative[F]
      override def settings: S = s
      override def outMonoid: Monoid[O] = Monoid[O]
      override def errors: ExtruderErrors[F] = ExtruderErrors[F]
    }
}
