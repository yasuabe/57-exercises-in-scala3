package exercises.ex15

import cats.data.StateT
import cats.syntax.flatMap._
import cats.syntax.functor._
import munit.FunSuite
import exercises.common.Pair._
import exercises.common.InOut._
import exercises.common.{ Std, given }
import exercises.common.StdState

class Solution02Test extends FunSuite:
  import StdState.{*, given }

  val sut = new Solution02[StdState]{}
  val dict = Map(("ok_user", "abc$123"))

  test("accept existing user with correct password") {
    // ARRANGE / ACT
    val inOut: InOut = InOut("ok_user", "abc$123")
    val Right((Nil, out)) = sut.exec(dict).runS(inOut).map(_.unapply)

    //ASSERT
    assertEquals(out, List(
      "What is your username?",
      "What is the password?",
      "Welcome!"))
  }
  test("reject existing user with incorrect password") {
    // ARRANGE / ACT
    val inOut: InOut = InOut("ok_user", "AbC$123")
    val Right((Nil, _ :: _ :: msg :: Nil)) = sut.exec(dict).runS(inOut).map(_.unapply)

    //ASSERT
    assertEquals(msg, "I don't know you.")
  }
  test("reject unknown user") {
    // ARRANGE / ACT
    val inOut: InOut = InOut("ng_user", "aBc$123")
    val Right((Nil, _ :: _ :: msg :: Nil)) = sut.exec(dict).runS(inOut).map(_.unapply)

    //ASSERT
    assertEquals(msg, "I don't know you.")
  }

