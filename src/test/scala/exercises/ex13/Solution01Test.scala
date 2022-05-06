package exercises.ex13

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

  test("determin compound interest") {
    // ARRANGE / ACT
    val inOut: InOut = InOut("1500", "4.3", "6", "4")
    val Right((in, out)) = sut.exec.runS(inOut).map(_.unapply)

    //ASSERT
    assert(in.isEmpty)
    assertEquals(out, List(
      "Enter the principal amount:",
      "Enter the rate:",
      "Enter the the number of years:",
      "Enter the the number of times the interest is compounded per year:",
      """$1500 was invested at 4.3% for 6 years
        |compounded 4 times per years is $1938.84.""".stripMargin))
  }
