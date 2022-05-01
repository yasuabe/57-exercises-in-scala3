package exercises.ex08

import scala.util.Try
import cats.MonadError
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.syntax.apply._
import cats.effect.IO
import cats.effect.IOApp
import exercises.common.{ Std, given }

// exercise 08: Pizza Party
trait Solution01[F[_]]:
  def exec(using s: Std[F], m: MonadError[F, Throwable]): F[Unit] =
    val convert = (s: String) => m.catchNonFatal(s.toInt)
    val ask     = (q: String) => s.ask(q) >>= convert
 
    val buildOutput = (m: Int, p: Int) =>
      val n = p * 8
      s"""$m people with $p pizzas
         |Each person gets ${n / m} pieces of pizza.
         |There are ${n % m} left over pieces.""".stripMargin

    ( "How many people? ",
      "How many pizzas do you have? "
    ) .map[[X] =>> F[Int]]([T] => (t: T) => ask(t.toString))
      .mapN(buildOutput)
      .flatMap(s.println)

object Solution01 extends IOApp.Simple, Solution01[IO] :
  def run: IO[Unit] = exec
