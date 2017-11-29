package extruder.csv

import cats.Monoid
import cats.instances.unit._
import extruder.core._
import extruder.effect.ExtruderMonadError

trait CSVDecoders extends Decoders with PrimitiveDecoders with DerivedDecoders with DecodeTypes {
  override type Eff[F[_]] = ExtruderMonadError[F]
  override type Hint = CSVHints
  override type InputData = Iterable[String]
  override type DecodeData = (Map[List[String], Int], List[String])
  override type Dec[F[_], T] = CSVDecoder[F, T]

  override protected def lookupValue[F[_]](path: List[String], data: (Map[List[String], Int], List[String]))(
    implicit hints: Hint,
    F: Eff[F]
  ): F[Option[String]] = F.pure(data._1.get(path).flatMap(data._2.lift))

  override protected def mkDecoder[F[_], T](
    f: (List[String], Option[T], (Map[List[String], Int], List[String])) => F[T]
  ): CSVDecoder[F, T] = new CSVDecoder[F, T] {
    override def read(path: List[String], default: Option[T], input: (Map[List[String], Int], List[String])) =
      f(path, default, input)
  }
}

trait CSVDecoder[F[_], T] extends Decoder[F, T, (Map[List[String], Int], List[String])]

object CSVDecoder extends CSVDecoders

trait CSVHints extends Hints

object CSVHints extends HintsCompanion[CSVHints] {
  override implicit def default: CSVHints = new CSVHints {
    override def pathToString(path: List[String]): String = path.mkString(".")
  }
}

class CSVSource[F[_], T <: Product]()(implicit val params: Parameters[T]) extends CSVDecoders {
  lazy val headers: Map[List[String], Int] = params.eval(List.empty).map(_.path).zipWithIndex.toMap
  lazy val headersSize: Int = headers.size

  def decode(input: List[String])(implicit decoder: Dec[F, T], F: Eff[F], hints: Hint): F[T] =
    if (input.size != headersSize)
      F.raiseError(new RuntimeException(s"Row '${input.mkString(",")}' is not of size '$headersSize'"))
    else decoder.read(List.empty, None, (headers, input))
}

trait CSV[G[_], V] {
  type Eff[F[_]] = ExtruderMonadError[F]

  def decodeAll[F[_], T <: Product](
    input: V,
    source: CSVSource[F, T]
  )(implicit decoder: CSVDecoder[F, T], F: Eff[F], hints: CSVHints): F[G[T]] =
    F.map(decodeAllState[F, T, Unit](input, source, (_, _) => ()))(_._2)

  def decodeAllState[F[_], T <: Product, S](
    input: V,
    source: CSVSource[F, T],
    updateState: (F[(S, G[T])], F[T]) => S
  )(implicit decoder: CSVDecoder[F, T], F: Eff[F], hints: CSVHints, monoid: Monoid[S]): F[(S, G[T])]
}

trait CSVSync[V] extends CSV[Iterable, V] {
  def splitInput(input: V): Iterable[List[String]]

  override def decodeAllState[F[_], T <: Product, S](
    input: V,
    source: CSVSource[F, T],
    updateState: (F[(S, Iterable[T])], F[T]) => S
  )(implicit decoder: CSVDecoder[F, T], F: Eff[F], hints: CSVHints, monoid: Monoid[S]): F[(S, Iterable[T])] =
    splitInput(input).foldLeft[F[(S, Iterable[T])]](F.pure((monoid.empty, Iterable.empty[T]))) { (acc, v) =>
      val decoded: F[T] = source.decode(v)
      val newState: S = updateState(acc, decoded)
      val newAcc: F[(S, Iterable[T])] = F.map(acc) { case (s, x) => (monoid.combine(s, newState), x) }
      F.flatMap(F.map(F.attempt(decoded))(_.fold[T](_ => return newAcc, identity)))(
        vv =>
          F.map(newAcc) {
            case (s, a) => (s, a ++ Iterable(vv))
        }
      )
    }
}

object CSVIterable extends CSVSync[Iterable[List[String]]] {
  override def splitInput(input: Iterable[List[String]]): Iterable[List[String]] = input
}