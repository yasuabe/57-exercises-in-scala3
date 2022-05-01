package exercises.ex04

import cats.Monad
import cats.syntax.flatMap._
import cats.syntax.traverse._
import cats.effect.IO
import cats.effect.IOApp
import exercises.common.{ Std, given }

def exec[F[_]](using e: Std[F], m: Monad[F]): F[Unit] =
  List("noun", "verb", "adjective", "adverb")
    .traverse(t => e.ask(s"Enter a $t: "))
    .flatMap { (_: @unchecked) match {
        case  n :: v :: adj :: adv :: Nil =>
           e.println(s"Do you $v your $adj $n $adv? That's hilarious")
      }
    }

object Solution01 extends IOApp.Simple :
  def run: IO[Unit] = exec[IO]