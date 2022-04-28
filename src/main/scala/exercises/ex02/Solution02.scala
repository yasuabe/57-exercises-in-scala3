package exercises.ex02

import cats.syntax.flatMap._
import cats.effect.IO
import cats.effect.IOApp

// put error message when empty input string
object Solution02 extends IOApp.Simple :
  import IO._

  def input: IO[String] = print("What is the input string? ") >> readLine
  def output(input: String): IO[Unit] = 
    val result = if (input.isEmpty) then "Please enter non-empty string"
                                    else s"$input has ${input.length} characters"
    println(result)

  def run: IO[Unit] = input >>= output