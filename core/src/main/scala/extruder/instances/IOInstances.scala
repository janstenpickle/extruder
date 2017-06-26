package extruder.instances

import cats.effect.IO
import extruder.core.{ExtruderApplicativeError, IOConvert, IOF, IOFlatMap, Hints}

import scala.concurrent.ExecutionContext

trait IOInstances {
  import ExtruderApplicativeError._

  implicit def extruderApplicativeErrorForIo(implicit ec: ExecutionContext): ExtruderApplicativeError[IO, Throwable] =
    new FromMonadError[IO]() {
      override def attemptIO[A](a: IO[IO[A]])(implicit utils: Hints): IO[A] = a.flatMap(identity)
    }

  implicit val ioConvertForIo: IOConvert[IO] = new IOConvert[IO] {
    override def toIO[A](a: IO[A]): IO[A] = a
    override def fromIO[A](a: IO[A]): IO[A] = a
  }

  implicit val ioFlatMapForIo: IOFlatMap[IO] = new IOFlatMap[IO]() {
    override def flatMap[A, B](fa: IOF[IO, A])(f: (A) => IOF[IO, B]): IOF[IO, B] =
      fa.flatMap(_.flatMap(f))
  }
}
