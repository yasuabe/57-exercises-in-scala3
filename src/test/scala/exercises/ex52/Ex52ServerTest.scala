package exercises.ex52

import scala.concurrent.duration.{ FiniteDuration, MILLISECONDS }
import cats.Applicative
import cats.data.StateT
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.effect.Clock
import munit.FunSuite
import exercises.common.Pair._
import exercises.common.InOut._
import exercises.common.{ Std, given }
import exercises.common.StdState
import StdState.{*, given }
import io.circe.syntax.*

class Ex52ServerTest extends FunSuite:
  val sut = new Ex52Server[StdState]{}

  test("calculate the year of retirement") {
    // ARRANGE / ACT
    val inOut: InOut = InOut()
    val Right(json) = sut.currentTime.runA(inOut)

    //ASSERT
    assertEquals(
      json.toString,
      """{
        |  "currentTime" : "2050-01-24 15:06:26"
        |}""".stripMargin)
  }

// always 15:06:26 UTC January 4 2050.
given Clock[StdState] with
  def applicative: Applicative[StdState] = summon[Applicative[StdState]]
  def monotonic: StdState[FiniteDuration] = ???
  def realTime: StdState[FiniteDuration] = applicative.pure(FiniteDuration(2526649586000L, MILLISECONDS))