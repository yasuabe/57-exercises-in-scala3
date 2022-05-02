package exercises.ex08

import cats.MonadError
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.effect.IO
import cats.effect.IOApp
import exercises.common.{ Std, given }
import scala.util.chaining._

// exercise 08: Pizza Party
trait Solution01[F[_]]:
  def exec(using s: Std[F], m: MonadError[F, Throwable]): F[Unit] =
    val convert = (s: String) => m.catchNonFatal(s.toInt)
    val ask     = (o: String) => s.ask(o) >>= convert
 
    val build = (m: Int, p: Int) =>
      val n = p * 8
      s"""$m people with $p pizzas
         |Each person gets ${n / m} pieces of pizza.
         |There are ${n % m} left over pieces.""".stripMargin

    for {
      m <- ask("How many people? ")
      p <- ask("How many pizzas do you have? ")
      _ <- build(m, p) pipe s.println
    } yield ()

object Solution01 extends IOApp.Simple, Solution01[IO] :
  def run: IO[Unit] = exec
