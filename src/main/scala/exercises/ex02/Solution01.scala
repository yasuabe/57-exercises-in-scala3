package exercises.ex02

import cats.FlatMap
import cats.syntax.flatMap.*
import cats.effect.IO
import cats.effect.IOApp
import exercises.common.{ Std, given }

// exercise 02: Counting the Number of Characters
trait Solution01[F[_]: FlatMap]:
  def exec(using e: Std[F]): F[Unit] =
    val input = e.ask("What is the input string? ")
    val output = (input: String) => e.println(s"$input has ${input.length} characters")

    input >>= output

object Solution01 extends IOApp.Simple, Solution01[IO]:
  def run: IO[Unit] = exec