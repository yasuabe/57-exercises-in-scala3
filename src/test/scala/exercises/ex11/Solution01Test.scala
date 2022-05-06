package exercises.ex11

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

  test("convert euro to dollar") {
    // ARRANGE / ACT
    val inOut: InOut = InOut("81", "137.51")
    val Right((Nil, out)) = sut.exec.runS(inOut).map(_.unapply)

    //ASSERT
    assertEquals(out, List(
      "How many euros are you exchanging?",
      "What is the exchange rate?",
      """81 euros at an exchange rate of 137.51 is
        |111.38 U.S. dollars.""".stripMargin))
  }
