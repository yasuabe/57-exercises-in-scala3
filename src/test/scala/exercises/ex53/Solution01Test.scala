package exercises.ex53

import cats.data.StateT
import cats.syntax.functor.*
import cats.syntax.applicative.*
import munit.FunSuite
import exercises.common.StdState
import exercises.common.InOut._
import exercises.common.{ Std, given }

class Solution01Test extends FunSuite:
  import StateT.*
  import StdState.{*, given }

  val sut = new Solution01[StdState]{}
  val std = summon[Std[StdState]]

  test("list all tasks") {
    // ARRANGE
    given KVStore[StdState] with
      def list: StdState[List[Task]] = List(Task(1, "todo1"), Task(2, "todo2")).pure[StdState]
      def add(todo: Todo): StdState[Task] = ???
      def delete(id: Int): StdState[Unit] = ???

    // ACT
    val Right(out) = sut.list.runA(InOut())

    //ASSERT
    assertEquals(out, List(Task(1, "todo1"), Task(2, "todo2")))
  }
  test("add a task") {
    // ARRANGE
    given KVStore[StdState] with
      def list: StdState[List[Task]] = ???
      def add(td: Todo): StdState[Task] = Task(0, td).pure[StdState]
      def delete(id: Int): StdState[Unit] = ???

    // ACT
    val Right(out) = sut.addTask("todo2").runA(InOut())

    //ASSERT
    assertEquals(out, Task(0, "todo2")
    )
  }
  test("don't add empty todo") {
    // ARRANGE
    given KVStore[StdState] with
      def list: StdState[List[Task]] = ???
      def add(td: Todo): StdState[Task] = Task(0, td).pure[StdState]
      def delete(id: Int): StdState[Unit] = ???

    // ACT
    val Left(out) = sut.addTask("").runA(InOut())

    //ASSERT
    assertEquals(out.getMessage, "bad request")
  }
  test("remove a task") {
    // ARRANGE
    given KVStore[StdState] with
      def list: StdState[List[Task]] = ???
      def add(todo: Todo): StdState[Task] = ???
      def delete(id: Int): StdState[Unit] = std.print(s"$id").void

    // ACT
    val Right(state, out) = sut.delete(1).run(InOut())

    //ASSERT
    assertEquals(out, ())
    assertEquals(state.out, List("1"))
  }