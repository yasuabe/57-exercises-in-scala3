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

def exec[F[_]](using e: Std[F], m: MonadError[F, Throwable]): F[Unit] =
  val askCurrentAge    = e.ask("What is your current age? ")
  val askRetirementAge = e.ask("At what age would you like to retire? ")

  val convert     = (s: String) => m.fromTry(Try(s.toInt))
  val currentYear = LocalDate.now.getYear.pure

  val buildOutput = (d: Int, y1: Int, y2: Int) =>
    s"You have $d left until you can retire. It's $y1, so you can retire in $y2"

  val result = for {
    ca <- askCurrentAge    >>= convert
    ra <- askRetirementAge >>= convert
    cy <- currentYear
  } yield buildOutput(ra - ca, cy, cy + (ra - ca))

  result >>= e.println

object Solution01 extends IOApp.Simple :
  def run: IO[Unit] = exec[IO]