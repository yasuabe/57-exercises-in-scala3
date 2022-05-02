package exercises.ex04

import cats.FlatMap
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.effect.IO
import cats.effect.IOApp
import exercises.common.{ Std, given }

// exercise 04: Mad Lib
trait Solution01[F[_]: FlatMap]:
  def exec(using s: Std[F]): F[Unit] =
    val ask = (t: String) => s.ask(s"Enter a $t: ")
    for {
      n   <- ask("noun")
      v   <- ask("verb")
      adj <- ask("adjective")
      adv <- ask("adverb")
      _   <- s.println(s"Do you $v your $adj $n $adv? That's hilarious")
    } yield ()

object Solution01 extends IOApp.Simple, Solution01[IO]:
  def run: IO[Unit] = exec