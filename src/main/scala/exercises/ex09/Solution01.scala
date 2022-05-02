package exercises.ex09

import cats.MonadError
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.effect.IO
import cats.effect.IOApp
import exercises.common.{ Std, given }
import scala.util.chaining.*

// exercise 09: Paint Calculator
trait Solution01[F[_]]:
  import Solution01.*
  def exec(using s: Std[F], m: MonadError[F, Throwable]): F[Unit] =
    val convert = (s: String) => m.catchNonFatal(s.toInt)
    val ask     = (o: String) => s.ask(o) >>= convert
    val divMod  = (n: Int, m: Int) => (n / m, n % m)
 
    val build = (l: Int, w: Int) =>
      val a      = l * w
      val (d, m) = divMod(a, AreaPerGallon)
      val g      = if m == 0 then d else d + 1
      s"""You will need to purchase $g gallons of
         |paint to cover $a square feet.""".stripMargin

    for {
      l <- ask("What is the length of the ceiling in feet? ")
      w <- ask("What is the width of the ceiling in feet? ")
      _ <- build(l, w) pipe s.println
    } yield ()

object Solution01 extends IOApp.Simple, Solution01[IO] :
  val AreaPerGallon = 350
  def run: IO[Unit] = exec
