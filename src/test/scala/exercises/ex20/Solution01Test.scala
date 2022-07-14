package exercises.ex20

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

  test("For Eau Claire resident") {
    // ARRANGE / ACT
    val Right((Nil, out)) = act("10", "Wisconsin", "Eau Claire")

    //ASSERT
    assertEquals(out, List(
      "What is the order amount?",
      "What state do you live in?",
      "What county do you live in?",
      "The tax is $0.55.",
      "The total is $10.55."))
  }
  test("For Dunn resident") {
    // ARRANGE / ACT
    val Right((Nil, out)) = act("10", "Wisconsin", "Dunn")

    //ASSERT
    assertEquals(out.drop(2), List(
      "What county do you live in?",
      "The tax is $0.54.",
      "The total is $10.54."))
  }
  test("For residents living in other counties in Wisconsin") {
    // ARRANGE / ACT
    val Right((Nil, out)) = act("10", "Wisconsin", "BUFFALO")

    //ASSERT
    assertEquals(out.drop(2), List(
      "What county do you live in?",
      "The tax is $0.50.",
      "The total is $10.50."))
  }
  test("For Illinois residents") {
    // ARRANGE / ACT
    val Right((Nil, out)) = act("10", "Illinois")

    //ASSERT
    assertEquals(out.drop(2), List(
      "The tax is $0.80.",
      "The total is $10.80."))
  }
  test("For residents in neither Wisconsin nor Illinois") {
    // ARRANGE / ACT
    val Right((Nil, out)) = act("10", "Hawaii")

    //ASSERT
    assertEquals(out.drop(2), List(
      "The total is $10.00."))
  }