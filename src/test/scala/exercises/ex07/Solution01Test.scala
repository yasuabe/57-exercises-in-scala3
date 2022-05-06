package exercises.ex07

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

  test("calculate area of rectangualr room") {
    // ARRANGE / ACT
    val inOut: InOut = InOut("15", "20")
    val Right((Nil, out)) = sut.exec.runS(inOut).map(_.unapply)

    //ASSERT
    assertEquals(out, List(
      "What is the length of the room in feet?",
      "What is the width of the room in feet?",
      """You entered dimensions of 15 feet by 20 feet.
        |The area is
        |300 square feet
        |27.871 square meters""".stripMargin))
  }
