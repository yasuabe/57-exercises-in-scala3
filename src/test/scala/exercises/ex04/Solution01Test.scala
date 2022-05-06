package exercises.ex04

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

  test("apply +, -, *, / on given two numbers") {
    // ARRANGE / ACT
    val inOut: InOut = InOut("dog", "walk", "blue", "quickly")
    val Right((Nil, out)) = sut.exec.runS(inOut).map(_.unapply)

    //ASSERT
    assertEquals(out, List(
      "Enter a noun:"     ,  // dog
      "Enter a verb:"     ,  // walk
      "Enter a adjective:",  // blue
      "Enter a adverb:"   ,  // quickly
      "Do you walk your blue dog quickly? That's hilarious"))
  }
