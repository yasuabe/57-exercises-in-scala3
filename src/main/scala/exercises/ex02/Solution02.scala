package exercises.ex02

import cats.syntax.flatMap._
import cats.FlatMap
import cats.effect.IO
import cats.effect.IOApp
import exercises.common.{ Std, given }

// put error message when entered empty input string
def exec[F[_]: FlatMap](using e: Std[F]): F[Unit] =
  val input = e.ask("What is the input string? ")
  val output = (input: String) => e.println(
    if (input.isEmpty)
        then "Please enter non-empty string"
        else s"$input has ${input.length} characters"
    )
  input >>= output

object Solution02 extends IOApp.Simple :
  def run: IO[Unit] = exec[IO]