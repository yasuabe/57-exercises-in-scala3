package exercises.ex06

import java.time.LocalDate
import cats.MonadError
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.syntax.applicative.*
import cats.effect.IO
import cats.effect.IOApp
import cats.effect.Clock
import exercises.common.{ Std, given }
import scala.util.chaining.*
import scala.concurrent.duration.FiniteDuration

// exercise 06: Retirement Calculator
trait Solution01[F[_]]:
  import Solution01.toYear

  def exec(using s: Std[F], m: MonadError[F, Throwable], c: Clock[F]): F[Unit] =
    val convert     = (s: String) => m.catchNonFatal(s.toInt)
    val ask         = [T] => (o: T) => s.ask(o.toString) >>= convert
    val currentYear = c.realTime.map(toYear)

    val build = (d: Int, y1: Int, y2: Int) =>
      s"""You have $d years left until you can retire.
         |It's $y1, so you can retire in $y2""".stripMargin

    for {
      ca <- ask("What is your current age? ")
      ra <- ask("At what age would you like to retire? ")
      cy <- currentYear
      _  <- build(ra - ca, cy, cy + (ra - ca)) pipe s.println
    } yield ()

object Solution01 extends IOApp.Simple, Solution01[IO]:
  def toYear(d: FiniteDuration): Int =
    import java.time.*
    Instant.ofEpochMilli(d.toMillis).atZone(ZoneId.systemDefault).toLocalDate.getYear

  def run: IO[Unit] = exec