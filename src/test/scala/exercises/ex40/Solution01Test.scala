package exercises.ex40

import munit.FunSuite
import exercises.common.InOut._
import exercises.common.StdState
import StdState.{*, given }

class Solution01Test extends FunSuite:
  val sut = new Solution01[StdState]{}

  def act(ss: String*) = sut.exec.runS(InOut(ss*)).map(_.unapply)

  test("works following given scenario") {
    // ARRANGE / ACT
    val Right((in, out)) = act("Jac")

    //ASSERT
    assert(in.isEmpty, in.toString)
    assertEquals(out, List(
      "Enter a search string:",
      "Results:",
      "Name                | Position          | Separation Date",
      "--------------------|-------------------|----------------",
      "Jacquelyn Jackson   | DBA               |",
      "Jake Jacobson       | Programmer        |"))
  }
  test("return empty table when not matched") {
    // ARRANGE / ACT
    val Right((in, out)) = act("NONONO")

    //ASSERT
    assert(in.isEmpty, in.toString)
    assertEquals(out, List(
      "Enter a search string:",
      "Results:",
      "Name    | Position        | Separation Date",
      "-------|-----------------|----------------"))
  }
  test("return all records for empty search word") {
    // ARRANGE / ACT
    val Right((in, out)) = act("")

    //ASSERT
    assert(in.isEmpty, in.toString)
    assertEquals(out, List(
      "Enter a search string:",
      "Results:",
      "Name                  | Position                   | Separation Date",
      "----------------------|----------------------------|----------------",
      "Jacquelyn Jackson     | DBA                        |",
      "Jake Jacobson         | Programmer                 |",
      "John Johnson          | Manager                    | 2016-12-31",
      "Michaela Michaelson   | District Manager           | 2015-12-19",
      "Sally Weber           | Web Developer Weber        | 2015-12-18",
      "Tou Xiong             | Software Engineer          | 2016-10-05"))
  }