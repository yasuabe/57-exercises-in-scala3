package exercises.ex27

import munit.FunSuite
import exercises.common.InOut._
import exercises.common.StdState
import StdState.{*, given }

class Solution01Test extends FunSuite:
  val sut = new Solution01[StdState]{}

  def act(ss: String*) = sut.exec.runS(InOut(ss*)).map(_.unapply)

  test("report all inputs are invalid") {
    // ARRANGE / ACT
    val Right((Nil, out)) = act("J", "", "ABCDE", "A12-1234")

    //ASSERT
    assertEquals(out, List(
      "Enter the first name:",
      "Enter the last name:",
      "Enter the ZIP code:",
      "Enter an employee ID:",
      """"J" is not a valid first name. It is too short.""",
      "The last name must be filled in.",
      "The ZIP code must be numeric.",
      "A12-1234 is not a valid ID."))
  }
  test("report all other inputs are invalid") {
    // ARRANGE / ACT
    val Right((Nil, out)) = act("x", "x", "x", "x")

    //ASSERT
    assertEquals(out.drop(4), List(
      """"x" is not a valid first name. It is too short.""",
      """"x" is not a valid last name. It is too short.""",
      "The ZIP code must be numeric.",
      "x is not a valid ID."))
  }
  test("report all inputs are valid") {
    // ARRANGE / ACT
    val Right((Nil, out)) = act("Jimmy", "James", "55555", "TK-4321")

    //ASSERT
    assertEquals(out, List(
      "Enter the first name:",
      "Enter the last name:",
      "Enter the ZIP code:",
      "Enter an employee ID:",
      "There are no errors are found"))
  }
