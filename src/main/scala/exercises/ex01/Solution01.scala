package exercises.ex01

import cats.syntax.flatMap._
import cats.effect.IO
import cats.effect.IOApp

object Solution01 extends IOApp.Simple :
  def input: IO[String] = IO.print("What is your name? ") >> IO.readLine
  def concat(name: String): IO[String] = IO.pure(s"Hello, $name, nice to meet you!")
  def output(greeting: String): IO[Unit] = IO.println(greeting)

  def run: IO[Unit] = input >>= concat >>= output