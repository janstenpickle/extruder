package extruder.stream

import cats.syntax.functor._
import extruder.core._
import extruder.effect.ExtruderMonadError
import shapeless.Sized.wrap
import shapeless._
import shapeless.ops.nat._

final case class HeaderData(paramIndex: Map[List[String], Int], headers: Option[List[String]] = None)

trait StreamDecoders extends Decoders with PrimitiveDecoders with DerivedDecoders with DecodeTypes {
  override type Eff[F[_]] = ExtruderMonadError[F]
  override type Hint = StreamHints
  override type DecodeData = (HeaderData, List[String])
  override type Dec[F[_], T] = StreamDecoder[F, T]

  override protected def lookupValue[F[_]](path: List[String], data: (HeaderData, List[String]))(
    implicit hints: Hint,
    F: Eff[F]
  ): F[Option[String]] = F.pure(hints.emptyString(data._1.paramIndex.get(path).flatMap(data._2.lift)))

  override protected def mkDecoder[F[_], T](
    f: (List[String], Option[T], (HeaderData, List[String])) => F[T]
  ): StreamDecoder[F, T] =
    new StreamDecoder[F, T] {
      override def read(path: List[String], default: Option[T], input: (HeaderData, List[String])) =
        f(path, default, input)
    }

  override def missingError[F[_], T](
    path: List[String],
    data: (HeaderData, List[String])
  )(implicit hints: StreamHints, F: ExtruderMonadError[F]): F[T] =
    data._1.headers.fold[F[T]](super.missingError(path, data))(
      h =>
        F.missing(
          s"Could not find value for '${h(data._1.paramIndex.keys.toList.indexOf(path))}' and no default available"
      )
    )

  override protected def parserError[F[_], T](
    path: List[String],
    value: String,
    err: String,
    data: (HeaderData, List[String])
  )(implicit hints: StreamHints, F: ExtruderMonadError[F]): F[T] =
    data._1.headers.fold[F[T]](super.parserError(path, value, err, data))(
      h =>
        F.validationFailure(
          s"Could not parse value '$value' for '${h(data._1.paramIndex.keys.toList.indexOf(path))}' and no default available: $err"
      )
    )
}

trait StreamDecoder[F[_], T] extends Decoder[F, T, (HeaderData, List[String])]

object StreamDecoder extends StreamDecoders

trait StreamHints extends Hints {
  def emptyString(value: Option[String]): Option[String]
  def checkHeaders(headersSize: Int, input: List[String]): Boolean
}

object StreamHints extends HintsCompanion[StreamHints] {
  override implicit def default: StreamHints = new StreamHints {
    override def pathToString(path: List[String]): String = path.mkString(".")
    override def emptyString(value: Option[String]): Option[String] = value.filterNot(_.isEmpty)
    override def checkHeaders(headersSize: Int, input: List[String]): Boolean = input.lengthCompare(headersSize) < 0
  }
}

class StreamSource[F[_], T] private (val header: HeaderData) extends StreamDecoders {
  lazy val headersSize: Int = header.paramIndex.size

  def decode(input: List[String])(implicit decoder: Dec[F, T], F: Eff[F], hints: Hint): F[T] =
    if (hints.checkHeaders(headersSize, input))
      F.validationFailure(s"Row length '${input.mkString(",")}' is smaller than number of headers '$headersSize'")
    else decoder.read(List.empty, None, (header, input))
}

object StreamSource {
  private def paramsToMap[T](params: Parameters[T]): Map[List[String], Int] =
    params.eval(List.empty).map(_.path).zipWithIndex.toMap

  def apply[F[_], T](implicit params: Parameters[T], pl: ValueLength[T], hints: StreamHints): StreamSource[F, T] =
    new StreamSource(HeaderData(paramsToMap(params)))

  def makeSized[F[_], A, N <: Nat](
    r: List[A],
    err: String
  )(implicit ti: ToInt[N], F: ExtruderMonadError[F]): F[Sized[List[A], N]] =
    if (r.lengthCompare(ti()) == 0) F.pure(wrap[List[A], N](r))
    else F.validationFailure(s"$err: ${r.size} is not equal to ${ti()}")

  def callCons[F[_], T, V](mappings: List[V], err: String)(
    implicit vl: ValueLength[T],
    F: ExtruderMonadError[F]
  ): (Sized[List[V], vl.Len] => StreamSource[F, T]) => F[StreamSource[F, T]] = f => {
    implicit val ti: ToInt[vl.Len] = vl.toInt
    makeSized[F, V, vl.Len](mappings, err).map(f)
  }

  def mappedHeaders[F[_], T, N <: Nat](
    headers: Sized[List[String], N]
  )(implicit params: Parameters[T], vl: ValueLength.Aux[T, N]): StreamSource[F, T] =
    new StreamSource[F, T](HeaderData(paramsToMap(params), Some(headers.unsized)))

  def mappedHeaders[F[_], T](
    headers: List[String]
  )(implicit params: Parameters[T], vl: ValueLength[T], F: ExtruderMonadError[F]): F[StreamSource[F, T]] =
    callCons[F, T, String](headers, "Incorrect headers sized provided").apply(mappedHeaders(_)(params, vl))

  def mappedIndex[F[_], T, N <: Nat](
    indexMapping: Sized[List[Int], N]
  )(implicit params: Parameters[T], vl: ValueLength.Aux[T, N]): StreamSource[F, T] =
    new StreamSource[F, T](HeaderData(paramsToMap(params).mapValues(indexMapping.unsized(_))))

  def mappedIndex[F[_], T](
    indexMapping: List[Int]
  )(implicit params: Parameters[T], vl: ValueLength[T], F: ExtruderMonadError[F]): F[StreamSource[F, T]] = {
    implicit val ti: ToInt[vl.Len] = vl.toInt
    makeSized[F, Int, vl.Len](indexMapping, "Incorrect mapping size provided").map(mappedIndex(_)(params, vl))
  }

  def mappedHeadersIndex[F[_], T, N <: Nat](
    headerMapping: Sized[List[(String, Int)], N]
  )(implicit params: Parameters[T], pl: ValueLength.Aux[T, N]): StreamSource[F, T] =
    new StreamSource[F, T](
      HeaderData(
        paramsToMap(params).mapValues(headerMapping.map(_._2).unsized(_)),
        Some(headerMapping.map(_._1).unsized)
      )
    )

  def mappedHeadersIndex[F[_], T](
    headerMapping: List[(String, Int)]
  )(implicit params: Parameters[T], vl: ValueLength[T], F: ExtruderMonadError[F]): F[StreamSource[F, T]] =
    //callCons[F, T, ]
    {
      implicit val ti: ToInt[vl.Len] = vl.toInt

      makeSized[F, (String, Int), vl.Len](headerMapping, "Incorrect header name and mapping size provided")
        .map(mappedHeadersIndex(_)(params, vl))
    }
}
