package exercises.ex23

import cats.MonadThrow
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.syntax.applicative.*
import cats.syntax.applicativeError.*
import cats.effect.IO
import cats.effect.IOApp
import exercises.common.{ Std, given }

// exercise 23: Troubleshooting Car Issues
trait Solution01[F[_]: MonadThrow]:
  import Solution01.*
  import Node.*

  def exec(using s: Std[F]): F[Unit] =
    val convert: String => F[Boolean] = s => s.toLowerCase match
      case "y" => true.pure
      case "n" => false.pure
      case _   => Exception("invalid input").raiseError

    val ask: String => F[Boolean] = (prompt: String) =>
      (s.ask(prompt) >>= convert).attempt.map(_.toOption).untilDefinedM

    extension (n: Node) def evaluate: F[String] = n match
      case Question(q, y, n) => ask(q).ifM(y.evaluate, n.evaluate)
      case Answer(a)         => a.pure
      case Undefined         => "undefined".pure

    Question("Is the car silent when you turn the key? ",
    y = Question("Are the battery terminals corroded? ",
        y = Answer("Clean terminals and try starting again"),
        n = Answer("Replace cables and try again." )),
    n = Question("Does the car make a clicking noise? ",
        y = Answer("Replace the battery."),
        n = Question("Does the car crank up but fail to start? ",
            y = Answer("Check spark plug connections"),
            n = Question("Does the engine start and then die? ",
                y = Question("Does your car have fuel injection? ",
                    n = Answer("Check to ensure the choke is opening and closing."),
                    y = Answer("Get it in for service.")),
                n = Undefined)))).evaluate >>= s.println

object Solution01 extends IOApp.Simple, Solution01[IO]:
  enum Node:
    case Question(q: String, y: Node, n: Node)
    case Answer(msg: String)
    case Undefined

  def run: IO[Unit] = exec