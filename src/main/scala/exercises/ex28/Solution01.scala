package exercises.ex28

import cats.MonadThrow
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.syntax.traverse.*
import cats.syntax.applicativeError.*
import cats.effect.{ IO, IOApp }
import exercises.common.{ Std, given }

// exercise 28: Adding Numbers
trait Solution01[F[_]]:
  def exec(using s: Std[F], m: MonadThrow[F]): F[Unit] =
    val askNumber = s.ask("Enter a number: ") >>= (s => m.catchNonFatal(s.toInt))

    val input = LazyList
     .continually(askNumber)
     .take(5)
     .sequence
     .map(_.toList.sum)

    val output = (n: Int) => s.println(s"The total is $n.")

    (input >>= output).handleErrorWith(t => s.println(s"Error: ${t.getMessage}"))

object Solution01 extends IOApp.Simple, Solution01[IO]:
  def run: IO[Unit] = exec