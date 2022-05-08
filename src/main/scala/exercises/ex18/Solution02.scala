package exercises.ex18

import cats.MonadError
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.syntax.compose._
import cats.syntax.applicativeError._
import cats.syntax.apply._
import cats.effect.IO
import cats.effect.IOApp
import exercises.common.{ Std, given }

// exercise 18: Temperature Converter
//     Challenge: "Don’t allow the user to proceed if the value entered is not numeric."
trait Solution02[F[_]]:
  import Solution01.*
  def exec(using s: Std[F], m: MonadError[F, Throwable]): F[Unit] =
    val ask = [T] => (prompt: String, convert: String => T) =>
      val f = s.ask(prompt) >>= (s => m.catchNonFatal(convert(s)).attempt.map(_.toOption))
      m.untilDefinedM(f)

    val inputs:F[Double] = 
      val printDesc = s.println(
        """Press C to convert from Fahrenheit to Celsius.
          |Press F to convert from Celsius to Fahrenheit.""".stripMargin)

      val askCOrF = ask("Your choice: ", Type.valueOf)

      val askTemperature = (cOrF: Type) =>
        ask(s"Please enter the temperature in ${cOrF.invert.repl}: ", _.toDouble)

      for {
        _    <- printDesc
        cOrF <- askCOrF
        temp <- askTemperature(cOrF)
      } yield cOrF.convert(temp)

    val buildResult: Double => String = temp =>
      f"The temperature in Celsius is $temp%.1f."

    inputs >>= (buildResult >>> s.println)

object Solution02 extends IOApp.Simple, Solution02[IO]:
  enum Type(inv: => String, val repl: String, val convert: Double => Double):
    def invert: Type = Type.valueOf(inv)
    case C extends Type("F", "Celsius"   , f => (f - 32) * 5 / 9)
    case F extends Type("C", "Fahrenheit", c => c * 9 / 5 + 32  )

  def run: IO[Unit] = exec
