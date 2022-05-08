package exercises.ex18

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

  def act(ss: String*) = sut.exec.runS(InOut(ss*)).map(_.unapply)

  test("repeat until getting either C to F") {
    // ARRANGE / ACT
    val Right((Nil, out)) = act("X", "", "F", "0")

    //ASSERT

    assertEquals(out, List(
      """Press C to convert from Fahrenheit to Celsius.
        |Press F to convert from Celsius to Fahrenheit.""".stripMargin,
      "Your choice:",
      "Your choice:",
      "Your choice:",
      "Please enter the temperature in Celsius:",
      "The temperature in Celsius is 32.0."))
  }
  test("repeat until getting proper temperature") {
    // ARRANGE / ACT
    val Right((Nil, out)) = act("C", "", "non temperatur", "32")

    //ASSERT

    assertEquals(out, List(
      """Press C to convert from Fahrenheit to Celsius.
        |Press F to convert from Celsius to Fahrenheit.""".stripMargin,
      "Your choice:",
      "Please enter the temperature in Fahrenheit:",
      "Please enter the temperature in Fahrenheit:",
      "Please enter the temperature in Fahrenheit:",
      "The temperature in Celsius is 0.0."))
  }

