package exercises.ex28

import munit.FunSuite
import exercises.common.InOut._
import exercises.common.StdState
import StdState.{*, given }

class Solution02Test extends FunSuite:
  val sut = new Solution02[StdState]{}

  def act(ss: String*) = sut.exec.runS(InOut(ss*)).map(_.unapply)

  test("report sum of five input numbers") {
    // ARRANGE / ACT
    val Right((Nil, out)) = act("3", "1", "2", "3")

    //ASSERT
    assertEquals(out, List(
      "How many numbers to add:",
      "Enter a number:",
      "Enter a number:",
      "Enter a number:",
      "The total is 6."))
  }