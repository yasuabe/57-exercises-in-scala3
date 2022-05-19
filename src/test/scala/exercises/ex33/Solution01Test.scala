package exercises.ex33

import munit.FunSuite
import exercises.common.InOut._
import exercises.common.StdState
import StdState.{*, given }
import exercises.common.states.StateHolder
import cats.data.StateT
import cats.syntax.flatMap._
import cats.syntax.functor._
import exercises.common.{ Std, given }

class Solution01Test extends FunSuite:
  type ThrowableOr     = [T] =>> Either[Throwable, T]
  type StringIntHolder = StateHolder[String | Int, String]
  type StdState        = [T] =>> StateT[ThrowableOr, StringIntHolder, T]

  val sut = new Solution01[StdState]{}

  import Solution01.Random
  import cats.data.State

  given Std[StdState] with
    import StateT.*
    def print(s: String): StdState[Unit] = get.flatMap(io => set(io push s.trim))
    def readLine: StdState[String] = get[ThrowableOr, StringIntHolder] >>= {
      _.pop.fold(throw AssertionError("no input")){
        (line, newIo) => set[ThrowableOr, StringIntHolder](newIo).as(line.toString)
      }
    }

  given Random[StdState] with
    def nextNonNega(max: Int): StdState[Int] = for {
      state0      <- StateT.get
      (m, state1) =  state0.pop.getOrElse(throw new AssertionError("no value"))
      _           <- StateT.set(state1)
    } yield m match 
      case n: Int => n
      case _      => throw new AssertionError("got non-integer")

  def act(ss: (String|Int)*) = sut.exec.runS(StateHolder.apply(ss*)).map(_.unapply)

  test("works following given scenario") {
    // ARRANGE / ACT
    val Right((in, out)) = act("Will I be rich and famous?", 3)

    //ASSERT
    assert(in.isEmpty, in.toString)
    assertEquals(out, List(
      "What's your question?",
      "Ask again later."))
  }
  test("choose reply using given randam") {
    val whenGiven = (n: Int) => act("", n).fold(throw _, _._2(1))
    assertEquals("Yes.", whenGiven(0))
    assertEquals("No.", whenGiven(1))
    assertEquals("Maybe.", whenGiven(2))
    assertEquals("Ask again later.", whenGiven(3))
  }