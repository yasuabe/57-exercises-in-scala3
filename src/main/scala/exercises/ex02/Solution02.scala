package exercises.ex02

import cats.FlatMap
import cats.syntax.flatMap.*
import cats.effect.IO
import cats.effect.IOApp
import exercises.common.{ Std, given }

// exercise 02: Counting the Number of Characters
// challenge 1: put error message when entered empty input string
trait Solution02[F[_]: FlatMap]:
  def exec(using e: Std[F]): F[Unit] =
    val input = e.ask("What is the input string? ")
    val output = (input: String) => e.println(
      if (input.isEmpty)
          then "Please enter non-empty string"
          else s"$input has ${input.length} characters"
      )
    input >>= output

object Solution02 extends IOApp.Simple, Solution02[IO]:
  def run: IO[Unit] = exec