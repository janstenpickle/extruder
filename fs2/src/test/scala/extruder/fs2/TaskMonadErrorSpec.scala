package extruder.fs2

import cats.Eq
import cats.instances.all._
import cats.laws.discipline.CartesianTests.Isomorphisms
import cats.laws.discipline.MonadErrorTests
import extruder.core.TestCommon._
import extruder.fs2.Fs2TestStrategy._
import fs2.Task
import org.scalacheck.Arbitrary
import org.specs2.Specification
import org.specs2.specification.core.SpecStructure
import org.typelevel.discipline.specs2.Discipline

import scala.util.Try

class TaskMonadErrorSpec extends Specification with Discipline {
  import TaskMonadErrorSpec._

  override def is: SpecStructure = checkAll("Task Monad Error", MonadErrorTests[Task, Throwable].monadError[Int, Int, Int])
}

object TaskMonadErrorSpec {
  implicit def taskArb[A](implicit arb: Arbitrary[A]): Arbitrary[Task[A]] = Arbitrary(arb.arbitrary.map(Task.now))
  implicit def taskEq[A](implicit e: Eq[A]): Eq[Task[A]] = new Eq[Task[A]] {
    override def eqv(x: Task[A], y: Task[A]): Boolean = tryEq[A].eqv(Try(x.unsafeRun()), Try(y.unsafeRun()))
  }
  implicit val throwableEq: Eq[Throwable] = Eq[String].on(_.toString)

  implicit val taskIso: Isomorphisms[Task] = Isomorphisms.invariant[Task](taskMonadError)
}