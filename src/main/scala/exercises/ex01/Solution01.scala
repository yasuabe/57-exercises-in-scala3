package exercises.ex01

import cats.syntax.flatMap._
import cats.Monad
import cats.effect.IO
import cats.effect.IOApp
import exercises.common.{ Std, given }

def exec[F[_]](using e: Std[F], m: Monad[F]): F[Unit] =
  def input: F[String] = e.ask("What is your name? ")
  def concat(name: String): F[String] = m.pure(s"Hello, $name, nice to meet you!")
  def output(greeting: String): F[Unit] = e.println(greeting)

  input >>= concat >>= output

object Solution01 extends IOApp.Simple :
  def run: IO[Unit] = exec[IO]