package exercises.ex19

import cats.MonadError
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.syntax.applicativeError.*
import cats.effect.IO
import cats.effect.IOApp
import exercises.common.{ Std, given }

// exercise 19: BMI Calculator
trait Solution01[F[_]]:
  import Solution01.*
  def exec(using s: Std[F], m: MonadError[F, Throwable]): F[Unit] =
    val ask = [T] => (prompt: String, convert: String => T) =>
      val f = s.ask(prompt) >>= (s => m.catchNonFatal(convert(s)).attempt.map(_.toOption))
      m.untilDefinedM(f)

    val inputs:F[(Double, Double)] = m.product(
      ask("Height in inches: ", _.toDouble),
      ask("Weight in pounds: ", _.toDouble))

    val calcBMI: ((Double, Double)) => Double =
      (h, w) => w / (h * h) * 703

    val printBMI: Double => F[Unit] =
      bmi => s.println(f"Your BMI is $bmi%.1f.")

    val printMessage: Double => F[Unit] = bmi =>
      val message = if (18.5 <= bmi && bmi <= 25)
        then f"You are within the ideal weight range."
        else f"You are overweight. You should see your doctor."
      s.println(message)

    for {
      bmi <- inputs map calcBMI
      _   <- printBMI(bmi)
      _   <- printMessage(bmi)
    } yield ()

object Solution01 extends IOApp.Simple, Solution01[IO]:

  def run: IO[Unit] = exec
