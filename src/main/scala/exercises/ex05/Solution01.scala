package exercises.ex05

import cats.MonadError
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.syntax.monadError._
import cats.effect.IO
import cats.effect.IOApp
import exercises.common.{ Std, given }
import scala.util.chaining._

// exercise 05: Simple Math
trait Solution01[F[_]]:
  def exec(using s: Std[F], m: MonadError[F, Throwable]): F[Unit] =
    val operations: List[(Char, (Int, Int) => Int)] = List(
      ('+', _ + _),
      ('-', _ - _),
      ('*', _ * _),
      ('/', _ / _))

    val convert   = (n: String) => m.catchNonFatal(n.toInt)
    val askNumber = (m: String) => s.ask(s"What is the $s number? ") >>= convert
    val build = (f: Int, s: Int) =>
      operations.map((o, b) => s"$f $o $s = ${b(f, s)}").mkString("\n")

    for {
      fst <- askNumber("first")
      snd <- askNumber("second")
      _   <- build(fst, snd) pipe s.println
    } yield ()

object Solution01 extends IOApp.Simple, Solution01[IO]:
  def run: IO[Unit] = exec