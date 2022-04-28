package exercises.ex02

import cats.FlatMap
import cats.syntax.flatMap._
import cats.syntax.applicative._
import cats.syntax.option._
import cats.effect.IO
import cats.effect.IOApp

// loop until non-empty input string
object Solution03 extends IOApp.Simple :
  import IO.{print, println, readLine}

  def input(using fm: FlatMap[IO]): IO[String] = 
    val confirm = (line: String) => if (line.isEmpty)
      then print("please enter non-empty string: ") as none
      else line.some.pure[IO]

    print("What is the input string? ") >> fm.untilDefinedM(readLine >>= confirm)

  def output(input: String): IO[Unit] = IO.println(s"$input has ${input.length} characters")
  def run: IO[Unit] = input >>= output