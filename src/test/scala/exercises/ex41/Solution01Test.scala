package exercises.ex41

import cats.Id
import munit.FunSuite
import fs2.{ Stream, Pipe, Pure, text }

class Solution01Test extends FunSuite:
  val sut = new Solution01[Pure]{}

  def act(ss: String) = sut.exec(Stream.emit[Pure, String](ss))

  test("works following given scenario") {
    // ARRANGE
    val in: String =
      """Ling, Mai
        |Johnson, Jim
        |Zarnecki, Sabrina
        |Jones, Chris
        |Jones, Aaron
        |Swift, Geoffrey
        |Xiong, Fong""".stripMargin

    // ACT
    val lines = act(in).toList

    //ASSERT
    assertEquals(lines, List(
      "Total of 7 names"  , "\n",
      "-----------------" , "\n",
      "Johnson, Jim"      , "\n",
      "Jones, Aaron"      , "\n",
      "Jones, Chris"      , "\n",
      "Ling, Mai"         , "\n",
      "Swift, Geoffrey"   , "\n",
      "Xiong, Fong"       , "\n",
      "Zarnecki, Sabrina" ))
  }