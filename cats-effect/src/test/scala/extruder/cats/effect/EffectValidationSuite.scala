package extruder.cats.effect

import java.io.{ByteArrayOutputStream, PrintStream}

import cats.Eq
import cats.data.EitherT
import cats.effect.{ContextShift, IO, Timer}
import cats.effect.laws.discipline._
import cats.effect.laws.util.{TestContext, TestInstances}
import cats.instances.all._
import cats.laws.discipline.SemigroupalTests.Isomorphisms
import extruder.data.ValidationErrors
import extruder.laws.ExtruderErrorsTests
import org.scalacheck.Arbitrary
import org.scalactic.source
import org.scalatest.{FunSuite, Matchers, Tag}
import org.typelevel.discipline.Laws
import org.typelevel.discipline.scalatest.Discipline

import scala.util.control.NonFatal
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.Success

class EffectValidationSuite extends FunSuite with Discipline with TestInstances with Matchers {

  type IOEV[A] = EffectValidation[IO, A]

  implicit val ioTimer: Timer[IO] = IO.timer(global)

  implicit def ioevArb[A](implicit arb: Arbitrary[A]): Arbitrary[IOEV[A]] =
    Arbitrary(arb.arbitrary.map(a => EffectValidation(EitherT[IO, ValidationErrors, A](IO(Right(a))))))

  implicit def ioArb[A](implicit arb: Arbitrary[A]): Arbitrary[IO[A]] = Arbitrary(arb.arbitrary.map(IO(_)))

  implicit def ioevEq[A: Eq](implicit ec: TestContext): Eq[IOEV[A]] = Eq.by(_.a.value)
  implicit def ioevETEq[A: Eq](implicit ec: TestContext): Eq[EitherT[IOEV, Throwable, A]] =
    Eq.by(_.value)

  implicit def iso: Isomorphisms[IOEV] = Isomorphisms.invariant[IOEV]

  checkAllAsync("EffectValidation", implicit ec => {
    implicit val cs = ec.contextShift[IO]
    ConcurrentEffectTests[IOEV].concurrentEffect[Int, Int, Int]
  })
  checkAllAsync("EffectValidation", implicit ec => {
    implicit val cs = ec.contextShift[IO]
    ConcurrentTests[IOEV].concurrent[Int, Int, Int]
  })
  checkAllAsync("EffectValidation", implicit ec => {
    implicit val cs = ec.contextShift[IO]
    EffectTests[IOEV].effect[Int, Int, Int]
  })
  checkAllAsync("EffectValidation", implicit ec => {
    implicit val cs = ec.contextShift[IO]
    AsyncTests[IOEV].async[Int, Int, Int]
  })
  checkAllAsync("EffectValidation", implicit ec => {
    implicit val cs = ec.contextShift[IO]
    SyncTests[IOEV].sync[Int, Int, Int]
  })

  checkAllAsync("EffectValidation", implicit ec => {
    ExtruderErrorsTests[IOEV].extruderErrors[Int]
  })

  test("Timer[EffectValidation].clock.realTime") {
    val time = System.currentTimeMillis()
    val io = implicitly[Timer[IOEV]].clock.realTime(MILLISECONDS)

    for (t2 <- io.value.unsafeToFuture()) yield {
      time should be > 0L
      time should be <= t2.right.getOrElse(0L)
    }
  }

  test("Timer[EffectValidation].clock.monotonic") {
    val time = System.nanoTime()
    val io = implicitly[Timer[IOEV]].clock.monotonic(NANOSECONDS)

    for (t2 <- io.value.unsafeToFuture()) yield {
      time should be > 0L
      time should be <= t2.right.getOrElse(0L)
    }
  }

  test("Timer[EffectValidation].sleep(10.ms)") {
    val t = implicitly[Timer[IOEV]]
    val io = for {
      start <- t.clock.monotonic(MILLISECONDS)
      _ <- t.sleep(10.millis)
      end <- t.clock.monotonic(MILLISECONDS)
    } yield {
      end - start
    }

    for (r <- io.value.unsafeToFuture()) yield {
      r.right.getOrElse(0L) should be > 0L
    }
  }

  test("Timer[EffectValidation].sleep(negative)") {
    val io = implicitly[Timer[IOEV]].sleep(-10.seconds).map(_ => 10)

    for (r <- io.value.unsafeToFuture()) yield {
      r.right.getOrElse(0) shouldBe 10
    }
  }

  testAsync("Timer[EffectValidation].shift") { ec =>
    implicit val cs = ec.contextShift[IO]
    val f = implicitly[ContextShift[IOEV]].shift.value.unsafeToFuture()

    f.value shouldBe None

    ec.tick()
    f.value shouldBe Some(Success(Right(())))
  }

  testAsync("Timer[EffectValidation].evalOn") { ec =>
    implicit val cs = ec.contextShift[IO]
    val cs2 = implicitly[ContextShift[IOEV]]
    val ec2 = TestContext()

    val f = cs2.evalOn(ec2)(EffectValidation(EitherT.liftF(IO(1)))).value.unsafeToFuture()
    f.value shouldBe None

    ec.tick()
    f.value shouldBe None

    ec2.tick()
    f.value shouldBe None
    ec.tick()
    f.value shouldBe Some(Success(Right(1)))
  }

  def testAsync[A](name: String, tags: Tag*)(f: TestContext => Unit)(implicit pos: source.Position): Unit =
    // Overriding System.err
    test(name, tags: _*)(silenceSystemErr(f(TestContext())))(pos)

  def checkAllAsync(name: String, f: TestContext => Laws#RuleSet): Unit = {
    val context = TestContext()
    val ruleSet = f(context)

    for ((id, prop) ← ruleSet.all.properties)
      test(name + "." + id) {
        silenceSystemErr(check(prop))
      }
  }

  def silenceSystemErr[A](thunk: => A): A = synchronized {
    // Silencing System.err
    val oldErr = System.err
    val outStream = new ByteArrayOutputStream()
    val fakeErr = new PrintStream(outStream)
    System.setErr(fakeErr)
    try {
      val result = thunk
      System.setErr(oldErr)
      result
    } catch {
      case NonFatal(e) =>
        System.setErr(oldErr)
        // In case of errors, print whatever was caught
        fakeErr.close()
        val out = outStream.toString("utf-8")
        if (out.nonEmpty) oldErr.println(out)
        throw e
    }
  }
}
