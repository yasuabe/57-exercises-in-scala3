package exercises.ex17

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

  test("recognize illegal BAC") {
    // ARRANGE / ACT
    val Right((Nil, out)) = act("1.49", "110", "M", "1")

    //ASSERT
    assertEquals(out, List(
      "Total alcohol consumed in oz:",
      "Body weight in lb:",
      "Gender:",
      "Number of hours since the last drink:",
      """Your BAC is 0.08
         |It is not legal for you to drive.""".stripMargin))
  }
  test("use all parameters") {
    // ARRANGE / ACT
    val Right((Nil, out)) = act("1", "150", "F", "3")

    //ASSERT
    assertEquals(out(4), 
      """Your BAC is 0.01
         |It is legal for you to drive.""".stripMargin)
  }
  test("reject unrecognizable gender input") {
    // ARRANGE / ACT
    val Left(t) = act("1.49", "110", "X", "1")

    //ASSERT
    assert(t.getMessage.contains("X"))
  }

