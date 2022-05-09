package exercises.ex23

import cats.data.StateT
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.syntax.monad._
import munit.FunSuite
import exercises.common.Pair._
import exercises.common.InOut._
import exercises.common.{ Std, given }
import exercises.common.StdState
import StdState.{*, given }

class Solution01Test extends FunSuite:
  val sut = new Solution01[StdState]{}

  def act(ss: String*) = sut.exec.runS(InOut(ss*)).map(_.unapply)

  test("Clean terminals and try starting again") {
    // ARRANGE / ACT
    val Right((Nil, out)) = act("y", "y")

    //ASSERT
    assertEquals(out, List(
      "Is the car silent when you turn the key?",
      "Are the battery terminals corroded?",
      "Clean terminals and try starting again"))
  }
  test("Replace cables and try again.") {
    // ARRANGE / ACT
    val Right((Nil, out)) = act("y", "n")

    //ASSERT
    assertEquals(out, List(
      "Is the car silent when you turn the key?",
      "Are the battery terminals corroded?",
      "Replace cables and try again."))
  }
  test("Replace the battery") {
    // ARRANGE / ACT
    val Right((Nil, out)) = act("n", "Y")

    //ASSERT
    assertEquals(out, List(
      "Is the car silent when you turn the key?",
      "Does the car make a clicking noise?",
      "Replace the battery."))
  }
  test("Check spark plug connections") {
    // ARRANGE / ACT
    val Right((Nil, out)) = act("N", "n", "y")

    //ASSERT
    assertEquals(out, List(
      "Is the car silent when you turn the key?",
      "Does the car make a clicking noise?",
      "Does the car crank up but fail to start?",
      "Check spark plug connections"))
  }
  test("Check to ensure the choke is opening and closing.") {
    // ARRANGE / ACT
    val Right((Nil, out)) = act("N", "n", "n", "y", "n")

    //ASSERT
    assertEquals(out, List(
      "Is the car silent when you turn the key?",
      "Does the car make a clicking noise?",
      "Does the car crank up but fail to start?",
      "Does the engine start and then die?",
      "Does your car have fuel injection?",
      "Check to ensure the choke is opening and closing."))
  }
  test("Get it in for service.") {
    // ARRANGE / ACT
    val Right((Nil, out)) = act("N", "n", "n", "y", "y")

    //ASSERT
    assertEquals(out, List(
      "Is the car silent when you turn the key?",
      "Does the car make a clicking noise?",
      "Does the car crank up but fail to start?",
      "Does the engine start and then die?",
      "Does your car have fuel injection?",
      "Get it in for service."))
  }
  test("undefined for n -> n -> n -> n") {
    // ARRANGE / ACT
    val Right((Nil, out)) = act("n", "n", "n", "n")

    //ASSERT
    assertEquals(out, List(
      "Is the car silent when you turn the key?",
      "Does the car make a clicking noise?",
      "Does the car crank up but fail to start?",
      "Does the engine start and then die?",
      "undefined"))
  }
