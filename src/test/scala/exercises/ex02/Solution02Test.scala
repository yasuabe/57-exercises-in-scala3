package exercises.ex02

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

  test("print number of chars") {
    // ARRANGE / ACT
    val inOut: InOut = InOut("")
    val Right((Nil, out)) = sut.exec.runS(inOut).map(_.unapply)

    //ASSERT
    assertEquals(out, List(
      "What is the input string?", // ""
      "Please enter non-empty string"))
  }
