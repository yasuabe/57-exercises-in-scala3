package exercises.ex19

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

  def act(ss: String*) = sut.exec.runS(InOut(ss*)).map(_.unapply)

  test("report ideal weight range") {
    // ARRANGE / ACT
    val Right((Nil, out)) = act("66.5", "122.6")

    //ASSERT
    assertEquals(out, List(
      "Height in inches:",
      "Weight in pounds:",
      "Your BMI is 19.5.",
      "You are within the ideal weight range."))
  }
  test("recommend doctor check") {
    // ARRANGE / ACT
    val Right((Nil, _ :: _ :: o1 :: o2 :: Nil)) = act("66", "222")

    //ASSERT
    assertEquals(o1, "Your BMI is 35.8.")
    assertEquals(o2, "You are overweight. You should see your doctor.")
  }
  test("continue asking until getting valid input") {
    // ARRANGE / ACT
    val Right((Nil, out)) = act("a", "66.5", "", "122.6")

    //ASSERT
    assertEquals(out.take(4), List(
      "Height in inches:",
      "Height in inches:",
      "Weight in pounds:",
      "Weight in pounds:"))
  }
