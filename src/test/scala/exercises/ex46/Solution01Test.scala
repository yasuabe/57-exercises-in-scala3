package exercises.ex46

import cats.Id
import munit.FunSuite
import fs2.{ Stream, Pipe, Pure, text }

class Solution01Test extends FunSuite:
  val sut = new Solution01[Pure]{}

  def act(ss: String) = sut.exec(Stream.emit[Pure, String](ss))

  test("works following given scenario") {
    // ARRANGE
    val in: String =
       """badger badger badger badger mushroom mushroom
         |snake badger badger badger""".stripMargin

    // ACT
    val lines = act(in).toList

    //ASSERT
    assertEquals(lines, List(
      "badger:   *******" , "\n",
      "mushroom: **"      , "\n",
      "snake:    *"       ))
  }