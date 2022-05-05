package exercises.ex15

import cats.MonadError
import cats.syntax.flatMap.*
import cats.syntax.option._
import cats.syntax.compose._
import cats.effect.IO
import cats.effect.IOApp
import exercises.common.{ Std, given }

// exercise 15: Password Validation]
trait Solution01[F[_]: [F[_]] =>> MonadError[F, Throwable]]:
  def exec(using s: Std[F], m: MonadError[F, Throwable]): F[Unit] =
    val askPassword = s.ask(s"What is the password? ")

    val validate = (password: String) =>
      if password == "abc$123" then "Welcome!" else "I don't know you."

    askPassword >>= (validate >>> s.println)

object Solution01 extends IOApp.Simple, Solution01[IO]:
  def run: IO[Unit] = exec
