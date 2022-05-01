package exercises.ex05

import scala.util.Try
import cats.MonadError
import cats.instances.either._
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.syntax.monadError._
import cats.syntax.applicativeError._
import cats.syntax.apply._
import cats.effect.IO
import cats.effect.IOApp
import exercises.common.{ Std, given }

def exec[F[_]](using e: Std[F], m: MonadError[F, Throwable]): F[Unit] =
  val operations: List[(Char, (Int, Int) => Int)] = List(
    ('+', _ + _),
    ('-', _ - _),
    ('*', _ * _),
    ('/', _ / _))

  def askNumber[T](s: T): F[String] = e.ask(s"What is the $s number? ")
  def convert(s: String): F[Int] = m.fromTry(Try(s.toInt))
  val buildOutput: (Int, Int) => String = (f, s) =>
    operations.map((o, b) => s"$f $o $s = ${b(f, s)}").mkString("\n")

  ("first", "second")
    .map[[X] =>> F[Int]]([T] => (t: T) => askNumber(t) >>= convert)
    .mapN(buildOutput)
    .flatMap(e.println)

object Solution01 extends IOApp.Simple :
  def run: IO[Unit] = exec[IO]