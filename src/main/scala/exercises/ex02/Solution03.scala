package exercises.ex02

import cats.Monad
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.syntax.applicative._
import cats.syntax.option._
import cats.effect.IO
import cats.effect.IOApp
import exercises.common.{ Std, given }

// exercise 02: Counting the Number of Characters
// challenge 2: loop waiting for non-empty input string
trait Solution03[F[_]]:
  def exec(using s: Std[F], m: Monad[F]): F[Unit] =
    val input = 
      val confirm = (line: String) => if (line.isEmpty)
        then s.print("please enter non-empty string: ") as none
        else line.some.pure[F]

      s.print("What is the input string? ") >> m.untilDefinedM(s.readLine >>= confirm)

    val output = (input: String) => s.println(s"$input has ${input.length} characters")

    input >>= output

object Solution03 extends IOApp.Simple, Solution03[IO]:
  def run: IO[Unit] = exec