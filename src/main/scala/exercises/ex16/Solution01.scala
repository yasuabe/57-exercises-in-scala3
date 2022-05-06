package exercises.ex16

import cats.MonadError
import cats.syntax.flatMap.*
import cats.syntax.option._
import cats.syntax.compose._
import cats.effect.IO
import cats.effect.IOApp
import exercises.common.{ Std, given }

// exercise 16: Legal Driving Age
trait Solution01[F[_]]:
  import Solution01.*
  def exec(using s: Std[F], m: MonadError[F, Throwable]): F[Unit] =
    val askAge = s.ask(s"What is your age? ") >>= (s => m.catchNonFatal(s.toInt))

    val buildResult: Int => String = age =>
      s"You are ${(age < 16) ?? "not " :: ""}old enough to legally drive."

    askAge >>= (buildResult >>> s.println)

object Solution01 extends IOApp.Simple, Solution01[IO] :
  extension [T](b: Boolean)
    def ?? (t: T): T => T = if b then (_: T) => t else identity

  extension [T](f: T => T) def :: (t: T): T = f(t)

  def run: IO[Unit] = exec
