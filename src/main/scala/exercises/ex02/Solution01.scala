package exercises.ex02

import cats.syntax.flatMap._
import cats.effect.IO
import cats.effect.IOApp

object Solution01 extends IOApp.Simple :
  import cats.effect.IO._

  def input: IO[String] = print("What is the input string? ") >> readLine
  def output(input: String): IO[Unit] = println(s"$input has ${input.length} characters")

  def run: IO[Unit] = input >>= output