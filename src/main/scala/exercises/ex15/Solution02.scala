package exercises.ex15

import cats.MonadError
import cats.data.Kleisli
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.effect.IO
import cats.effect.IOApp
import exercises.common.{ Std, given }

// exercise 15: Password Validation] (challenge 1)
trait Solution02[F[_]: [F[_]] =>> MonadError[F, Throwable]]:
  def exec(using s: Std[F], m: MonadError[F, Throwable]): Kleisli[F, Map[String, String], Unit] =
    import Kleisli.{ ask, liftF }

    val askNameAndPass = m.product(
      s.ask(s"What is your username? "),
      s.ask(s"What is the password? "))

    val validate = (dict: Map[String, String]) => (user: String, pass: String) =>
      dict
        .get(user)
        .filter(_ == pass)
        .as("Welcome!")
        .getOrElse("I don't know you.")

    for {
      inputs <- liftF(askNameAndPass)
      dict   <- ask[F, Map[String, String]]
      reply  =  validate(dict).tupled(inputs)
      _      <- liftF(s.println(reply))
    } yield ()

object Solution02 extends IOApp.Simple, Solution02[IO]:
  def run: IO[Unit] = exec.run(Map(("ok_user", "abc$123")))
