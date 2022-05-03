package exercises.ex11

import cats.MonadError
import cats.Monoid
import cats.syntax.flatMap.*
import cats.syntax.foldable.*
import cats.syntax.traverse.*
import cats.syntax.functor.*
import cats.syntax.apply.*
import cats.effect.IO
import cats.effect.IOApp
import exercises.common.{ Std, given }

// exercise 11: Currency Conversion
trait Solution01[F[_]]:
  def exec(using s: Std[F], m: MonadError[F, Throwable]): F[Unit] =
    val askRateAndRate =
      val ask = [T] => (prompt: String, conversion: String => T) =>
        s.ask(prompt) >>= (in => m.catchNonFatal(conversion(in)))

      m.product(
        ask(s"How many euros are you exchanging? ", _.toInt),
        ask(s"What is the exchange rate? "        , _.toDouble))

    val buildResult = (euro: Int, rate: Double) =>
      val dollar = scala.math.round(euro * rate)/ 100.0
      s"""$euro euros at an exchange rate of $rate is
         |$dollar U.S. dollars.""".stripMargin

    askRateAndRate.map((a, b) => buildResult(a, b)) >>= s.println

object Solution01 extends IOApp.Simple, Solution01[IO] :
  def run: IO[Unit] = exec
