package exercises.ex08

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

  test("divide pizzas") {
    // ARRANGE / ACT
    val inOut: InOut = InOut("8", "2")
    val Right((Nil, out)) = sut.exec.runS(inOut).map(_.unapply)

    //ASSERT
    assertEquals(out, List(
      "How many people?",
      "How many pizzas do you have?",
      """8 people with 2 pizzas
        |Each person gets 2 pieces of pizza.
        |There are 0 left over pieces.""".stripMargin))
  }
