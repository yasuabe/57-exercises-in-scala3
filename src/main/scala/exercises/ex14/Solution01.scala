package exercises.ex14

import cats.MonadError
import cats.syntax.flatMap.*
import cats.syntax.option._
import cats.syntax.compose._
import cats.effect.IO
import cats.effect.IOApp
import exercises.common.{ Std, given }

// exercise 14: Tax Calculator
trait Solution01[F[_]]:
  def exec(using s: Std[F], m: MonadError[F, Throwable]): F[Unit] =
    val ask = [T] => (t: String, f: String => T) =>
      s.ask(s"What is the $t? ") >>= (s => m.catchNonFatal(f(s)))

    val askAmount: F[Double]       = ask("order amount" , _.toDouble)
    val askState:  F[String]       = ask("state"        , identity)
    val inputs:F[(Double, String)] = m.product(askAmount, askState)

    // Constraint:
    //   "Implement this program using only a simple if statement — don’t use an else clause"
    val buildResult: (Double, String) => String = (amount, state) =>
      lazy val forWI = Option.when(state == "WI") { // 'if' equivalence
        val tax = (amount * 0.055)
        f"""The subtotal is $$$amount%.2f.
           |The tax is $$$tax%.2f.
           |The total is $$${amount + tax}%.2f.""".stripMargin
      }
      lazy val forOthers =
        val total = ((amount * 1.0) * 100).toInt / 100.0
        f"The total is $$$total%.2f".some

      List(forWI, forOthers).flatten.head

    inputs >>= (buildResult.tupled >>> s.println)

object Solution01 extends IOApp.Simple, Solution01[IO] :
  def run: IO[Unit] = exec
