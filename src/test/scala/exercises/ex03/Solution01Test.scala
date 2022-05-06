package exercises.ex03

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

  test("print quote and author") {
    // ARRANGE / ACT
    val inOut: InOut = InOut(
    "These aren't the droids you're looking for.",
    "Obi-Wan Kenobi")
    val Right((Nil, out)) = sut.exec.runS(inOut).map(_.unapply)

    //ASSERT
    assertEquals(out, List(
      "What is the quote?", 
      "Who said it?", 
      """Obi-Wan Kenobi says, "These aren't the droids you're looking for.""""))
  }
