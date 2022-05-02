package exercises.ex06

import scala.util.Try
import java.time.LocalDate
import cats.MonadError
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.syntax.applicative.*
import cats.syntax.apply.*
import cats.effect.IO
import cats.effect.IOApp
import exercises.common.{ Std, given }
import scala.util.chaining.*

// exercise 06: Retirement Calculator: Challenge 1
trait Solution02[F[_]]:
  def exec(using s: Std[F], m: MonadError[F, Throwable]): F[Unit] =
    val convert     = (s: String) => m.catchNonFatal(s.toInt)
    val ask         = (o: String) => s.ask(o.toString) >>= convert
    val currentYear = LocalDate.now.getYear.pure

    val build = (ca: Int, ra: Int, cy: Int) =>
      val ly = ra - ca
      if ly >= 0
        then s"""You have $ly left until you can retire.
           |It's $cy, so you can retire in ${cy + ly}.""".stripMargin
        else s"""You have already retired  ${-ly} years ago.
           |It's $cy, so you have retired in ${cy + ly}.""".stripMargin

    for {
      ca <- ask("What is your current age? ")
      ra <- ask("At what age would you like to retire? ")
      cy <- currentYear
      _  <- build(ca, ra, cy) pipe s.println
    } yield ()

object Solution02 extends IOApp.Simple, Solution02[IO] :
  def run: IO[Unit] = exec
