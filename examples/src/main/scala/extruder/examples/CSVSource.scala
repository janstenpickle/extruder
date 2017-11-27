package extruder.examples

import extruder.core._
import extruder.effect.ExtruderMonadError

trait CSVDecoders[F[_], T <: Product] extends Decoders with PrimitiveDecoders with DerivedDecoders with DecodeTypes {
  override type Eff[F[_]] = ExtruderMonadError[F]
  override type Hint = MapHints
  override type InputData = Iterable[String]
  override type DecodeData = List[String]
  override type Dec[F[_], V] = CSVDecoder[F, V]

  val params: Parameters[T]

  lazy val headers: Map[List[String], Int] = params.eval(List.empty).map(_.path).zipWithIndex.toMap

  override protected def lookupValue[F[_]](path: List[String], data: List[String])(
    implicit hints: Hint,
    F: Eff[F]
  ): F[Option[String]] = F.pure(headers.get(path).flatMap(data.lift))

  override protected def hasValue[F[_]](path: List[String], data: List[String])(
    implicit hints: Hint,
    F: Eff[F]
  ): F[Boolean] = F.map(lookupValue[F](path, data))(_.isDefined)

  override protected def mkDecoder[F[_], V](f: (List[String], Option[V], List[String]) => F[V]): Dec[F, V] =
    new CSVDecoder[F, V] {
      override def read(path: List[String], default: Option[V], input: List[String]) = f(path, default, input)
    }

  def decode(input: InputData)(implicit decoder: Dec[F, T], F: Eff[F], hints: Hint): F[T] =
    decoder.read(List.empty, None, input.toList)

  // decode each line accumilating errors in that line, but failing fast
  def decodeAll(input: Iterable[InputData])(implicit decoder: Dec[F, T], F: Eff[F], hints: Hint): F[Iterable[T]] =
    input.foldLeft(F.pure(Iterable.empty[T])) { (acc, v) =>
      val decoded = F.map(decode(v))(Iterable(_))
      F.flatMap(F.map(F.attempt(decoded))(_.fold[Iterable[T]](_ => return decoded, identity)))(
        vv => F.map(acc)(_ ++ vv)
      )
    }
}

trait CSVDecoder[F[_], T] extends Decoder[F, T, List[String]]

class CSVSource[F[_], T <: Product]()(override implicit val params: Parameters[T]) extends CSVDecoders[F, T]
