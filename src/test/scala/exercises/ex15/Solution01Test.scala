package exercises.ex15

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

  test("reject when given wrong password") {
    // ARRANGE / ACT
    val inOut: InOut = InOut("12345")
    val Right((Nil, out)) = sut.exec.runS(inOut).map(_.unapply)

    //ASSERT
    assertEquals(out, List(
      "What is the password?",
      "I don't know you."))
  }
  test("accept when given right password") {
    // ARRANGE / ACT
    val inOut: InOut = InOut("abc$123")
    val Right((Nil, _ :: msg :: Nil)) = sut.exec.runS(inOut).map(_.unapply)

    //ASSERT
    assertEquals(msg, "Welcome!")
  }
  test("reject case-sensitively") {
    // ARRANGE / ACT
    val inOut: InOut = InOut("aBc$123")
    val Right((Nil, _ :: msg :: Nil)) = sut.exec.runS(inOut).map(_.unapply)

    //ASSERT
    assertEquals(msg, "I don't know you.")
  }

