package exercises.ex09

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

  test("calculate the number of paint") {
    // ARRANGE / ACT
    val inOut: InOut = InOut("12", "30")
    val Right((Nil, out)) = sut.exec.runS(inOut).map(_.unapply)

    //ASSERT
    assertEquals(out, List(
      "What is the length of the ceiling in feet?",
      "What is the width of the ceiling in feet?",
      """You will need to purchase 2 gallons of
        |paint to cover 360 square feet.""".stripMargin))
  }
