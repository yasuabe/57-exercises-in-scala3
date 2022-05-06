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

class Solution02Test extends FunSuite:
  val sut = new Solution02[StdState]{}

  test("calculate retirement year for already retired") {
    // ARRANGE / ACT
    val inOut: InOut = InOut("25", "20")
    val Right((Nil, out)) = sut.exec.runS(inOut).map(_.unapply)

    //ASSERT
    assertEquals(out, List(
      "What is your current age?",
      "At what age would you like to retire?",
      """You have already retired  5 years ago.
        |It's 2022, so you have retired in 2017.""".stripMargin))
  }
