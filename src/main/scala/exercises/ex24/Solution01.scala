package exercises.ex24

import cats.MonadThrow
import cats.syntax.flatMap.*
import cats.syntax.option.*
import cats.syntax.functor.*
import cats.effect.IO
import cats.effect.IOApp
import exercises.common.{ Std, given }

// exercise 24: Anagram Checker
trait Solution01[F[_]: MonadThrow]:
  import Solution01.*

  case class DomainException(msg: String) extends Throwable

  def exec(using s: Std[F], m: MonadThrow[F]): F[Unit] =
    val validate: String => String => Boolean = s1 => s2 => s1.length == s2.length

    val askSecond = (first: String) => s.ask("Enter the second string:")
      .map(_.toOption(validate(first)))
      .untilDefinedM

    for {
      _      <- s.println("Enter two strings and I'll tell you if they are anagrams:")
      first  <- s.ask("Enter the first string:")
      second <- askSecond(first)
      nega   = if isAnagram(first, second) then "" else "not "
      _      <- s.println(s""""${first}" and "${second}" are ${nega}anagrams.""")
    } yield ()

object Solution01 extends IOApp.Simple, Solution01[IO]:
  extension [T](t: T) def toOption(f: T => Boolean): Option[T] = if (f(t)) then t.some else none[T]

  val isAnagram: (String, String) => Boolean = (x, y) => x.sorted == y.sorted

  def run: IO[Unit] = exec