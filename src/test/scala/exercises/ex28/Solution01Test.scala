package exercises.ex28

import munit.FunSuite
import exercises.common.InOut._
import exercises.common.StdState
import StdState.{*, given }

class Solution01Test extends FunSuite:
  val sut = new Solution01[StdState]{}

  def act(ss: String*) = sut.exec.runS(InOut(ss*)).map(_.unapply)

  test("report sum of five input numbers") {
    // ARRANGE / ACT
    val Right((Nil, out)) = act("1", "2", "3", "4",  "5")

    //ASSERT
    assertEquals(out, List(
      "Enter a number:",
      "Enter a number:",
      "Enter a number:",
      "Enter a number:",
      "Enter a number:",
      "The total is 15."))
  }
  test("report sum of other five input numbers") {
    // ARRANGE / ACT
    val Right((Nil, out)) = act("10", "9", "7", "4",  "0")

    //ASSERT
    assertEquals(out, List(
      "Enter a number:",
      "Enter a number:",
      "Enter a number:",
      "Enter a number:",
      "Enter a number:",
      "The total is 30."))
  }