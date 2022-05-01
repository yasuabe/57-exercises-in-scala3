package exercises.ex06

import scala.util.Try
import java.time.LocalDate
import cats.MonadError
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.syntax.applicative._
import cats.syntax.apply._
import cats.effect.IO
import cats.effect.IOApp
import exercises.common.{ Std, given }

def exec2[F[_]](using e: Std[F], m: MonadError[F, Throwable]): F[Unit] =
  val askCurrentAge    = e.ask("What is your current age? ")
  val askRetirementAge = e.ask("At what age would you like to retire? ")

  val convert     = (s: String) => m.fromTry(Try(s.toInt))
  val currentYear = LocalDate.now.getYear.pure

  val forActive = (d: Int, y1: Int, y2: Int) =>
    s"You have $d left until you can retire.\nIt's $y1, so you can retire in $y2"

  val forRetired = (d: Int, y1: Int, y2: Int) =>
    s"You have already retired $d years ago.\nIt's $y1, so you have retired in $y2"

  val result = for {
    ca <- askCurrentAge    >>= convert
    ra <- askRetirementAge >>= convert
    cy <- currentYear
  } yield {
    if ra - ca >= 0
    then forActive(ra - ca, cy, cy + (ra - ca))
    else forRetired(ca - ra, cy, cy + (ra -ca))
  }
  result >>= e.println

object Solution02 extends IOApp.Simple :
  def run: IO[Unit] = exec2[IO]