package exercises.ex17

import cats.MonadError
import cats.syntax.flatMap.*
import cats.syntax.compose._
import cats.syntax.apply._
import cats.effect.IO
import cats.effect.IOApp
import exercises.common.{ Std, given }

// exercise 17: Blood Alcohol Calculator
trait Solution01[F[_]]:
  import Solution01.*
  def exec(using s: Std[F], m: MonadError[F, Throwable]): F[Unit] =
    val ask = [T] => (prompt: String, convert: String => T) =>
      s.ask(prompt) >>= (s => m.catchNonFatal(convert(s)))

    val inputs:F[(Double, Double, Gender, Double)] = (
      ask("Total alcohol consumed in oz: "        , _.toDouble),
      ask("Body weight in lb: "                   , _.toDouble),
      ask("Gender: "                              , Gender.valueOf),
      ask("Number of hours since the last drink: ", _.toDouble)
    ).tupled

    val buildResult: (Double, Double, Gender, Double) => String = (a, w, g, h) =>
      val bac = a * 5.14 / (w * g.ratio) - 0.015 * h
      f"""Your BAC is $bac%.2f
         |It is ${if bac < 0.08 then "" else "not "}legal for you to drive.""".stripMargin

    inputs >>= (buildResult.tupled >>> s.println)

object Solution01 extends IOApp.Simple, Solution01[IO]:
  enum Gender(val ratio: Double):
    case M extends Gender(0.73) 
    case F extends Gender(0.66)

  def run: IO[Unit] = exec
