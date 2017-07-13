package extruder.core

import java.net.URL

import cats.effect.IO
import cats.instances.all.{catsStdEqForTry, _}
import cats.{Eq, Eval}
import org.scalacheck.{Arbitrary, Gen}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.concurrent.duration.{Duration, FiniteDuration}
import scala.util.Try

object TestCommon {
  val nonEmptyStringGen: Gen[String] = Gen.alphaNumStr.suchThat(_.nonEmpty)

  val namespaceGen: Gen[List[String]] = Gen.nonEmptyListOf(nonEmptyStringGen)

  val urlGen: Gen[URL] = for {
    host <- nonEmptyStringGen
    path <- nonEmptyStringGen
  } yield new URL(s"http://$host/$path")

  implicit val urlArb: Arbitrary[URL] = Arbitrary(urlGen)

  val finiteDurationGen: Gen[FiniteDuration] = Gen.chooseNum(0L, 5000L).map(Duration.fromNanos)

  val durationGen: Gen[Duration] =
    Gen.oneOf(finiteDurationGen, Gen.const(Duration.Inf), Gen.const(Duration.MinusInf), Gen.const(Duration.Zero))

  implicit def futureEq[A](implicit e: Eq[A]): Eq[Future[A]] = ioEq[A].on(fut => IO.fromFuture(Eval.later(fut)))

  implicit def tryEq[A](implicit e: Eq[A]) = catsStdEqForTry[A, Throwable](e, Eq[String].on(_.toString))

  implicit def ioEq[A](implicit e: Eq[A]): Eq[IO[A]] = new Eq[IO[A]] {

    override def eqv(a: IO[A], b: IO[A]): Boolean = {
      val a1 = Try(a.unsafeRunSync())
      val b1 = Try(b.unsafeRunSync())

      tryEq[A].eqv(a1, b1)
    }
  }
}
