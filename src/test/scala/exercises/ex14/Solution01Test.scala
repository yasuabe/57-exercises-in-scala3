package exercises.ex14

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

  test("calculate tax for WI") {
    // ARRANGE / ACT
    val inOut: InOut = InOut("10", "WI")
    val Right((in, out)) = sut.exec.runS(inOut).map(_.unapply)

    //ASSERT
    assert(in.isEmpty)
    assertEquals(out, List(
      "What is the order amount?",
      "What is the state?",
      """The subtotal is $10.00.
        |The tax is $0.55.
        |The total is $10.55.""".stripMargin))
  }
  test("do not calculate tax except for WI") {
    // ARRANGE / ACT
    val inOut = InOut("10", "NM")
    val Right((in, out)) = sut.exec.runS(inOut).map(_.unapply)

    //ASSERT
    assert(in.isEmpty)
    assertEquals(out, List(
      "What is the order amount?",
      "What is the state?",
      "The total is $10.00"))
  }
  test("throw error when failed to conversion") {
    // ARRANGE / ACT
    val inOut: InOut = InOut("not a number")
    val Left(t) = sut.exec.runS(inOut)

    //ASSERT
    assert(t.isInstanceOf[NumberFormatException])
  }
  
