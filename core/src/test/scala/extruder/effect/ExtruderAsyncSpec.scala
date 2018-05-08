package extruder.effect

import java.io.{ByteArrayOutputStream, PrintStream}

import cats.effect.laws.discipline.AsyncTests
import cats.effect.laws.util.TestContext
import cats.instances.all._
import org.specs2.scalacheck.Parameters
import org.specs2.specification.core.Fragments
import org.typelevel.discipline.Laws

import scala.util.control.NonFatal

abstract class ExtruderAsyncSpec[F[_], E](implicit F: ExtruderAsync[F]) extends EffectSpec[F, E] {
  override def ruleTest: Fragments = checkAllAsync("ExtruderAsync", implicit ec => AsyncTests[F].async[Int, Int, Int])

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

  def checkAllAsync(name: String, f: TestContext => Laws#RuleSet)(implicit p: Parameters): Fragments = {
    val context = TestContext()
    val ruleSet = f(context)

    s"""${ruleSet.name} laws must hold for ${name}""" ^ br ^
      Fragments.foreach(ruleSet.all.properties) {
        case (id, prop) =>
          id ! silenceSystemErr(check(prop, p, defaultFreqMapPretty)) ^ br
      }
  }
}
