package extruder.examples

import cats.data.EitherT
import cats.effect.IO
import extruder.core.{
  Decode,
  DecodeTypes,
  Decoder,
  Decoders,
  ExtruderApplicativeError,
  Hints,
  HintsCompanion,
  IOFlatMap,
  MapDataSource,
  MapDecoder,
  MapSource,
  Validation,
  ValidationErrors
}
import extruder.typesafe.{TypesafeConfigDecoder, TypesafeConfigSource}
import shapeless._

import shapeless.{:+:, CNil}

case class CombinedDecoder[F[_], E, T, D1[A[_], B] <: Decoder[A, B], D2[A[_], B] <: Decoder[A, B]](
  decoder1: D1[F, T],
  decoder2: D2[F, T]
)(implicit AE: ExtruderApplicativeError[F, E])
    extends Decoder[F, T] {
  override type DecodeData = (decoder1.DecodeData, decoder2.DecodeData)

//  override def read(path: List[String], default: Option[T], input: InputData): IO[F[T]] =
//    decoder1.read(path, default, input._1).flatMap { x =>
//      val iox = AE.map(x)(a => IO.pure(AE.pure(a)))
//      val xxx: F[IO[F[T]]] = AE.handleErrorWith(iox)(_ => AE.pure(decoder2.read(path, default, input._2)))
//
//      //AE.attemptT(iox).foldLeft(x)((a, b) => a)
//
//      val sda: EitherT[F, IO[F[T]], T] = AE
//
//      ???
//    }

  override def read(path: List[String], default: Option[T], input: DecodeData): IO[F[T]] =
    for {
      d1 <- decoder1.read(path, default, input._1)
      d2 <- decoder2.read(path, default, input._2)
    } yield AE.handleErrorWith(d1)(_ => d2)
}

object CombinedDecoder {

  implicit def apply[F[_], E, T, D1[A[_], B] <: Decoder[A, B], D2[A[_], B] <: Decoder[A, B]](
    implicit AE: ExtruderApplicativeError[F, E],
    d1: D1[F, T],
    d2: D2[F, T]
  ): CombinedDecoder[F, E, T, D1, D2] = CombinedDecoder(d1, d2)
}

object Hello extends App {

  println(
    implicitly[CombinedDecoder[Validation, ValidationErrors, Int, MapDecoder, MapDecoder]]
      .read(List("a", "b"), None, (Map.empty[String, String], Map("a.b" -> "1")))
      .unsafeRunSync()
  )

  type Cunt[F[_], T] = CombinedDecoder[F, ValidationErrors, T, MapDecoder, TypesafeConfigDecoder]

  implicitly[CombinedDecoder[Validation, ValidationErrors, Int, MapDecoder, Cunt]]
  //implicitly[Decoder.Aux[Validation, ValidationErrors, MapSource.DecodeData]]

  // println(MapSource.decodeIO[Int, Validation, ValidationErrors](Map.empty[String, String]), MapSource.decodeIO())
}

trait CDecoder[F[_], T, C] extends Decoder[F, T] {
  override type DecodeData = C
}

case class Combine[A <: Decode with DecodeTypes, B <: Decode with DecodeTypes](a: A, b: B) {
  //maybe have a hlist
  def runBoth[F[_], E, T](a: => IO[F[T]], b: => IO[F[T]])(implicit AE: ExtruderApplicativeError[F, E]): IO[F[T]] =
    for {
      aa <- a
      bb <- b
    } yield AE.handleErrorWith(aa)(_ => bb)

  def runBoth[T, F[_], E](path: List[String], input1: a.InputData, input2: b.InputData)(
    implicit AE: ExtruderApplicativeError[F, E]
  ): IO[F[T]] =
    runBoth(a.decodeIO[T, F, E](path, input1), b.decodeIO[T, F, E](path, input2))
}

//trait CombinedHints extends Hints
//
//object CombinedHints extends HintsCompanion[CombinedHints] {
//  override implicit def default: CombinedHints = new CombinedHints {
//    override def pathToString(path: List[String]): String = path.mkString(",")
//  }
//}
//
//case class CombinedSource[A <: Decode with DecodeTypes, B <: Decode with DecodeTypes](a: A, b: B) {
//
////
////  override type Dec[F[_], T] = MapDecoder[F, T] // CombinedDecoder[F, E, T, a.Dec, b.Dec]
//  type InputData = (a.InputData, b.InputData)
////  override type Hint = CombinedHints
//
////  def prepareInput[F[_], E](
////    namespace: List[String],
////    input: InputData
////  )(implicit AE: ExtruderApplicativeError[F, E], aHints: a.Hint, bHints: b.Hint) =
////    for {
////      aa <- a.prepareInput(namespace, input._1)
////      bb <- b.prepareInput(namespace, input._2)
////    } yield AE.ap2(AE.pure((x: a.DecodeData, y: b.DecodeData) => (x, y)))(aa, bb)
//
//  //a.prepareInput(namespace, input._1).flatMap(x => b.prepareInput(namespace, input._2).map)
//
//  //override protected def mkDecoder[F[_], E, T](f: (List[String], Option[T], DecodeData) => IO[F[T]]): Dec[F, T] = ???
//
//  def decode[T](input: InputData)(
//    implicit decoder1: a.Dec[Validation, T],
//    decoder2: b.Dec[Validation, T],
//    AE: ExtruderApplicativeError[Validation, ValidationErrors],
//    FM: IOFlatMap[Validation],
//    aHints: a.Hint,
//    bHints: b.Hint
//  ): Validation[T] =
//    a.decode[T, Validation, ValidationErrors](input._1)
//      .fold(_ => b.decode[T, Validation, ValidationErrors](input._2), AE.pure)
////
//  //override type Dec = this.type
//  //override type Hint = this.type
//}
//
//object CombinedSource {
//  type FuckYou[A <: Decode with DecodeTypes, B <: Decode with DecodeTypes] = A :+: CombinedSource[A, B] :+: CNil
//  def apply[A <: Decode with DecodeTypes, B <: Decode with DecodeTypes](a: A, b: B) = Coproduct.apply[FuckYou[A, B]](a)
//  def apply[A <: Decode with DecodeTypes, B <: Decode with DecodeTypes, C <: Decode with DecodeTypes](
//    a: A,
//    b: CombinedSource[B, C]
//  ) = Coproduct.apply[FuckYou[B, C]](b)
//}
