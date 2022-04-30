package exercises.ex02

import cats.Monad
import cats.syntax.flatMap._
import cats.effect.IO
import cats.effect.IOApp
import exercises.common.{ Std, given }

def exec[F[_]: Monad](using e: Std[F]): F[Unit] =
  val input = e.ask("What is the input string? ")
  val output = (input: String) => e.println(s"$input has ${input.length} characters")

  input >>= output
object Solution01 extends IOApp.Simple :
  def run: IO[Unit] = exec[IO]