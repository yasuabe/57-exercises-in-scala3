package exercises.ex06

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

  test("calculate the year of retirement") {
    // ARRANGE / ACT
    val inOut: InOut = InOut("25", "65")
    val Right((Nil, out)) = sut.exec.runS(inOut).map(_.unapply)

    //ASSERT
    assertEquals(out, List(
      "What is your current age?", // 25
      "At what age would you like to retire?", // 65
      """You have 40 years left until you can retire.
        |It's 2022, so you can retire in 2062""".stripMargin))
  }
