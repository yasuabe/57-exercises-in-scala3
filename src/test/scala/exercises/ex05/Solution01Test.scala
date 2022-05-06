package exercises.ex05

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

  test("apply +, -, *, / on given two numbers") {
    // ARRANGE / ACT
    val inOut: InOut = InOut("10", "5")
    val Right((Nil, out)) = sut.exec.runS(inOut).map(_.unapply)

    //ASSERT
    assertEquals(out, List(
      "What is the first number?",  // 10
      "What is the second number?", // 5
      """10 + 5 = 15
        |10 - 5 = 5
        |10 * 5 = 50
        |10 / 5 = 2""".stripMargin))
  }
