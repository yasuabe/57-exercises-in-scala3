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

class Solution01Test extends FunSuite:
  val sut = new Solution01[StdState]{}

  def act(ss: String*) = sut.exec.runS(InOut(ss*)).map(_.unapply)

  test("convert F to C") {
    // ARRANGE / ACT
    val Right((Nil, out)) = act("C", "32")

    //ASSERT
    assertEquals(out, List(
      """Press C to convert from Fahrenheit to Celsius.
        |Press F to convert from Celsius to Fahrenheit.
        |Your choice:""".stripMargin,
      "Please enter the temperature in Fahrenheit:",
      "The temperature in Celsius is 0.0."))
  }
  test("convert C to F") {
    // ARRANGE / ACT
    val Right((Nil, _ :: _ :: out :: Nil)) = act("F", "1.777")

    //ASSERT
    assertEquals(out, "The temperature in Celsius is 35.2.")
  }
