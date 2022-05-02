package exercises.ex03

import cats.FlatMap
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.effect.IO
import cats.effect.IOApp
import exercises.common.{ Std, given }

// exercise 03: Printing Quotes
trait Solution01[F[_]: FlatMap]:
  def exec(using s: Std[F]): F[Unit] =
    def output(quotation: String, author: String): F[Unit] =
      s.println(List(author, " says, \"", quotation, "\"").mkString)

    for {
      q <- s.ask("What is the quote?")
      a <- s.ask("Who said it?")
      _ <- output(q, a)
    } yield ()

object Solution01 extends IOApp.Simple, Solution01[IO]:
  def run: IO[Unit] = exec