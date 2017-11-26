package extruder.Monix

import cats.Eq
import cats.effect.laws.discipline.EffectTests
import cats.instances.all._
import cats.laws.discipline.CartesianTests.Isomorphisms
import extruder.core.TestCommon._
import extruder.monix.MonixInstances
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global
import org.scalacheck.Arbitrary
import org.specs2.Specification
import org.specs2.specification.core.SpecStructure
import org.typelevel.discipline.specs2.Discipline

import scala.concurrent.Await
import scala.concurrent.duration.Duration
import scala.util.Try

class TaskAsyncSpec extends Specification with Discipline {
  import TaskAsyncSpec._

  override def is: SpecStructure =
    checkAll("Task Effect", EffectTests[Task].effect[Int, Int, Int])
}

object TaskAsyncSpec extends MonixInstances {
  def runTask[A](a: Task[A]): Try[A] = Try(Await.result(a.runAsync, Duration.Inf))

  implicit def taskArb[A](implicit arb: Arbitrary[A]): Arbitrary[Task[A]] = Arbitrary(arb.arbitrary.map(Task(_)))
  implicit def taskEq[A](implicit e: Eq[A]): Eq[Task[A]] = new Eq[Task[A]] {
    override def eqv(x: Task[A], y: Task[A]): Boolean = if (tryEq[A].eqv(runTask(x), runTask(y))) true
    else {
      println(runTask(x))
      println(runTask(y))
      false
    }
  }
  implicit val throwableEq: Eq[Throwable] = Eq[String].on(_.toString)

  implicit val taskIso: Isomorphisms[Task] = Isomorphisms.invariant[Task](taskEffect)
}
