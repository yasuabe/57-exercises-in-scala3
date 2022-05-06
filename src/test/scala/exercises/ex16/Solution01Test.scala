package exercises.ex16

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

  def act(ss: String*) =
    sut.exec.runS(InOut(ss*)).map(_.unapply)

  test("15 is illegal") {
    // ARRANGE / ACT
    val Right((Nil, out)) = act("15")

    //ASSERT
    assertEquals(out, List(
      "What is your age?",
      "You are not old enough to legally drive."))
  }
  test("16 is legal") {
    val Right((Nil, _ :: msg :: Nil)) = act("16")
    assertEquals(msg, "You are old enough to legally drive.")
  }
  test("17 is legal") {
    val Right((Nil, _ :: msg :: Nil)) = act("17")
    assertEquals(msg, "You are old enough to legally drive.")
  }
