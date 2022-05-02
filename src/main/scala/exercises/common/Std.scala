package exercises.common

import cats.FlatMap
import cats.syntax.flatMap.*
import cats.effect.IO

trait Std[F[_]: FlatMap]:
  def print(s: String): F[Unit]
  def println(s: String): F[Unit] = print(s"$s\n")
  def readLine: F[String]
  def ask(prompt: String): F[String] = print(prompt) >> readLine

given Std[IO] with
  def print(s: String): IO[Unit] = IO.print(s)
  def readLine: IO[String] = IO.readLine
