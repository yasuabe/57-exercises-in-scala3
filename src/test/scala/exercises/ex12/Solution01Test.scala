package exercises.ex12

import cats.data.StateT
import cats.syntax.flatMap._
import cats.syntax.functor._
import munit.FunSuite
import exercises.common.Pair._
import exercises.common.InOut._
import exercises.common.{ Std, given }
import exercises.common.StdState
import StdState.{*, given }

class Solution01Test extends FunSuite:
  val sut = new Solution01[StdState]{}

  test("compute simple interest") {
    // ARRANGE / ACT
    val inOut: InOut = InOut("1500", "4.3", "4")
    val Right((Nil, out)) = sut.exec.runS(inOut).map(_.unapply)

    //ASSERT
    assertEquals(out, List(
      "Enter the principal:",
      "Enter the rate of interest:",
      "Enter the the number of years:",
      """After 4 years at 4.3%, the investment will
        |be worth $1755.0.""".stripMargin))
  }
