package exercises.ex47

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
  given AstrosService[StdState] with
    def retrieve(url: String): StdState[String] = std.print(url) >> std.readLine

  given Environment[StdState] with
    def url: StdState[String] = summon[Std[StdState]].readLine

  test("works following given scenario") {
    // ARRANGE
    val res = """{
      |  "people": [
      |    {
      |      "craft": "ISS",
      |      "name": "Gennady Padalka"
      |    }, {
      |      "craft": "ISS",
      |      "name": "Mikhail Kornienko"
      |    }, {
      |      "craft": "ISS",
      |      "name": "Scott Kelly"
      |    }
      |  ],
      |  "message": "success",
      |  "number": 3
      |}""".stripMargin
    val inOut: InOut = InOut("http://test", res)

    // ACT
    val Right((Nil, out)) = sut.exec.runS(inOut).map(_.unapply)

    //ASSERT
    assertEquals(out, List(
      "http://test",
      "There are 3 people in space right now:",
      "",
      "Name                | Craft",
      "--------------------|------",
      "Gennady Padalka     | ISS",
      "Mikhail Kornienko   | ISS",
      "Scott Kelly         | ISS"
    ))
  }
  test("works for another test fixture") {
    // ARRANGE
    val res = """{
      |  "people": [
      |    {
      |      "craft": "SSSSSSS",
      |      "name": "A"
      |    }
      |  ],
      |  "message": "success",
      |  "number": 1
      |}""".stripMargin
    val inOut: InOut = InOut("http://test2", res)

    // ACT
    val Right((Nil, out)) = sut.exec.runS(inOut).map(_.unapply)

    //ASSERT
    assertEquals(out, List(
      "http://test2",
      "There are 1 people in space right now:",
      "",
      "Name   | Craft",
      "-------|--------",
      "A      | SSSSSSS",
    ))
  }
  test("works for erroneous resuponse") {
    // ARRANGE
    val res = "error"
    val inOut: InOut = InOut("http://test2", res)

    // ACT
    val Left(e) = sut.exec.runS(inOut).map(_.unapply)

    //ASSERT
    assert(e.getMessage.contains("error"))
  }