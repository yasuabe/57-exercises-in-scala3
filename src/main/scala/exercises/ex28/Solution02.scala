package exercises.ex28

import cats.MonadThrow
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.syntax.traverse.*
import cats.syntax.applicativeError.*
import cats.effect.{ IO, IOApp }
import exercises.common.{ Std, given }

// exercise 28: Adding Numbers
//   Challenge: ask how many numbers to add
trait Solution02[F[_]]:
  def exec(using s: Std[F], m: MonadThrow[F]): F[Unit] =
    val askRepeat = s.ask("How many numbers to add: ") >>= (s => m.catchNonFatal(s.toInt))
    val askNumber = s.ask("Enter a number: ") >>= (s => m.catchNonFatal(s.toInt))

    val askNumbers = (n: Int) => LazyList
     .continually(askNumber)
     .take(n)
     .sequence
     .map(_.toList.sum)

    val output = (n: Int) => s.println(s"The total is $n.")

    askRepeat >>= askNumbers >>= output

object Solution02 extends IOApp.Simple, Solution02[IO]:
  def run: IO[Unit] = exec.handleErrorWith(t => IO.println(s"Error: ${t.getMessage}"))
