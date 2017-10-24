package extruder.examples

import cats.data.EitherT
import cats.effect.IO
import extruder.core.{DecodeTypes, Decoder, Decoders, ExtruderApplicativeError, Hints, HintsCompanion}
import shapeless._

//class CombinedDecoder[F[_], E, T](decoder1: Decoder[F, T], decoder2: Decoder[F, T])(
//  implicit AE: ExtruderApplicativeError[F, E]
//) extends Decoder[F, T] {
//  override type InputData = (decoder1.InputData, decoder2.InputData)
//
////  override def read(path: List[String], default: Option[T], input: InputData): IO[F[T]] =
////    decoder1.read(path, default, input._1).flatMap { x =>
////      val iox = AE.map(x)(a => IO.pure(AE.pure(a)))
////      val xxx: F[IO[F[T]]] = AE.handleErrorWith(iox)(_ => AE.pure(decoder2.read(path, default, input._2)))
////
////      //AE.attemptT(iox).foldLeft(x)((a, b) => a)
////
////      val sda: EitherT[F, IO[F[T]], T] = AE
////
////      ???
////    }
//  override def read(path: List[String], default: Option[T], input: InputData): IO[F[T]] =
//    for {
//      d1 <- decoder1.read(path, default, input._1)
//      d2 <- decoder2.read(path, default, input._2)
//    } yield AE.handleErrorWith(d1)(_ => d2)
//}

//object CombinedDecoder {
//  def apply[F[_], E, T, A <: HList](implicit AE: ExtruderApplicativeError[F, E]): CombinedDecoder[F, E, T] = ???
//}

trait CDecoder[F[_], T, C] extends Decoder[F, T] {
  override type InputData = C
}

trait CombinedHints extends Hints

object CombinedHints extends HintsCompanion[CombinedHints] {
  override implicit def default: CombinedHints = new CombinedHints {
    override def pathToString(path: List[String]): String = path.mkString(",")
  }
}

class CombinedSource[A, B] extends Decoders with DecodeTypes {

  override type DecodeData = (A, B)
  override type Dec[F[_], T] = CDecoder[F, T, DecodeData]
  override type InputData = (A, B)
  override type Hint = CombinedHints

  override protected def mkDecoder[F[_], T](f: (List[String], Option[T], DecodeData) => IO[F[T]]) = ???
}
