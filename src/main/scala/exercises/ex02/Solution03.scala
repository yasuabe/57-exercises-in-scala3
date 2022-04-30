package exercises.ex02

import cats.Monad
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.syntax.applicative._
import cats.syntax.option._
import cats.effect.IO
import cats.effect.IOApp
import exercises.common.{ Std, given }

def exec[F[_]](using e: Std[F], m: Monad[F]): F[Unit] =
  val input = 
    val confirm = (line: String) => if (line.isEmpty)
      then e.print("please enter non-empty string: ") as none
      else line.some.pure[F]

    e.print("What is the input string? ") >> m.untilDefinedM(e.readLine >>= confirm)

  val output = (input: String) => e.println(s"$input has ${input.length} characters")

  input >>= output

// loop waiting for non-empty input string
object Solution03 extends IOApp.Simple :
  def run: IO[Unit] = exec[IO]