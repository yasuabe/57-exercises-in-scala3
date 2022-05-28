package exercises.ex52

import cats.Id
import cats.data.StateT
import cats.syntax.flatMap.*
import munit.FunSuite
import exercises.common.StdState
import exercises.common.InOut._
import exercises.common.{ Std, given }

class Solution01Test extends FunSuite:
  import StateT.*
  import StdState.{*, given }

  val sut = new Solution01[StdState]{}

  val std = summon[Std[StdState]]
  given CTClient[StdState] with
    def retrieve(url: String): StdState[String] = std.print(url) >> std.readLine

  given Environment[StdState] with
    def url: StdState[String] = summon[Std[StdState]].readLine

  test("works following given scenario") {
    // ARRANGE
    val res = """{"currentTime" : "2050-01-04 15:06:26"}"""
    val inOut: InOut = InOut("http://test", res)

    // ACT
    val Right((Nil, out)) = sut.exec.runS(inOut).map(_.unapply)

    //ASSERT
    assertEquals(out, List(
      "http://test",
      "The current time is 15:06:26 UTC January 4 2050."
    ))
  }