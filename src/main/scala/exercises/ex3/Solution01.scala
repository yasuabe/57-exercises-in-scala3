package exercises.ex03

import cats.instances.string._
import cats.instances.tuple._
import cats.{ Foldable, FlatMap, Monad }
import cats.syntax.flatMap._
import cats.syntax.apply._
import cats.syntax.applicative._
import cats.syntax.functor._
import cats.syntax.foldable._
import cats.effect.IO
import cats.effect.IOApp
import exercises.common.{ Std, given }

def exec[F[_]](using e: Std[F], m: Monad[F]): F[Unit] =
  val askQuote  = e.ask("What is the quote?")
  val askAuthor = e.ask("Who said it?")
  def output(quotation: String, author: String): F[Unit] =
    e.println(List(author, " says, \"", quotation, "\"").mkString)

  m.product(askQuote, askAuthor) >>= output

object Solution01 extends IOApp.Simple :
  def run: IO[Unit] = exec[IO]