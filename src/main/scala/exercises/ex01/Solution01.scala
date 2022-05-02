package exercises.ex01

import cats.syntax.flatMap._
import cats.syntax.applicative._
import cats.Monad
import cats.effect.IO
import cats.effect.IOApp
import exercises.common.{ Std, given }

// exercise 01: Saying Hello
trait Solution01[F[_]: Monad]:
  def exec(using e: Std[F]): F[Unit] =
    def input: F[String] = e.ask("What is your name? ")
    def concat(name: String): F[String] = s"Hello, $name, nice to meet you!".pure
    def output(greeting: String): F[Unit] = e.println(greeting)

    input >>= concat >>= output

object Solution01 extends IOApp.Simple, Solution01[IO]:
  def run: IO[Unit] = exec