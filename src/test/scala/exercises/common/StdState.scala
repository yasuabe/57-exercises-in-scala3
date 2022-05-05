package exercises.common

import cats.data.StateT
import cats.syntax.flatMap._
import cats.syntax.functor._
import exercises.common.InOut._
import exercises.common.{ Std, given }

object StdState:
  type ThrowableOr = [T] =>> Either[Throwable, T]
  type StdState    = [T] =>> StateT[ThrowableOr, InOut, T]

  given Std[StdState] with
    import StateT.*
    def print(s: String): StdState[Unit] = get.flatMap(io => set(io push s))
    def readLine: StdState[String] = get[ThrowableOr, InOut] >>= {
      _.pop.fold(throw AssertionError("no input")){
        (line, newIo) => set[ThrowableOr, InOut](newIo).as(line)
      }
    }
