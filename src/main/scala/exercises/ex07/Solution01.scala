package exercises.ex07

import cats.MonadError
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.effect.IO
import cats.effect.IOApp
import exercises.common.{ Std, given }
import scala.util.chaining.*

// exercise 07: Area of a Rectangular Room
trait Solution01[F[_]]:
  val Ratio = 0.09290304
  def exec(using s: Std[F], m: MonadError[F, Throwable]): F[Unit] =
    val convert = (s: String) => m.catchNonFatal(s.toInt)
    val ask     = (o: String) => s.ask(o) >>= convert

    val build = (l: Int, w: Int) =>
      val af = l * w
      val am = af * Ratio
      f"""You entered dimensions of $l feet by $w feet.
         |The area is
         |$af square feet
         |$am%.3f square meters""".stripMargin

    for {
      l <- ask("What is the length of the room in feet? ")
      w <- ask("What is the width of the room in feet? ")
      _ <- build(l, w) pipe s.println
    } yield ()

object Solution01 extends IOApp.Simple, Solution01[IO]:
  def run: IO[Unit] = exec