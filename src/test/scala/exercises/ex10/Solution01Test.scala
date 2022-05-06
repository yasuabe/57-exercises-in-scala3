package exercises.ex10

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

  test("calculate tax-including total") {
    // ARRANGE / ACT
    val inOut: InOut = InOut("25", "2", "10", "1", "4", "1")
    val Right((Nil, out)) = sut.exec.runS(inOut).map(_.unapply)

    //ASSERT
    assertEquals(out, List(
      "Enter the price of item 1:",
      "Enter the quantity of item 1:",
      "Enter the price of item 2:",
      "Enter the quantity of item 2:",
      "Enter the price of item 3:",
      "Enter the quantity of item 3:",
      """Subtotal: $64.00
        |Tax: $3.52
        |Total: $67.52""".stripMargin))
  }
