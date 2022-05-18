package exercises.ex32

import scala.util.Try
import cats.MonadThrow
import cats.data.StateT
import cats.data.ReaderWriterStateT
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.syntax.option.*
import cats.syntax.traverse.*
import cats.syntax.applicativeError.*
import cats.effect.{ IO, IOApp }
import exercises.common.{ Std, given }
import Solution01.*

// exercise 32: Guess the Number Game
trait Solution01[F[_]: Std: MonadThrow]:

  def session(q: Int)(using S: Std[F], M: MonadThrow[F]): F[Int] =
    import ReaderWriterStateT.* 
    type RWS[T] = ReaderWriterStateT[F, Int, Int, String, T]

    val tryGuess: RWS[Option[Unit]] = 
      val askGuess: RWS[Int] = for {
        msg   <- get
        guess <- liftF(S.ask(msg) >>= (s => M.catchNonFatal(s.toInt)))
      } yield guess
      val checkGuess: Int => RWS[Option[String]] = n => ask.map { q =>
        if      n < q then "Too low. Guess again: ".some
        else if n > q then "Too high. Guess again: ".some
        else               none
      }
      val proceed  = (_: Option[String]).traverse(set[F, Int, Int, String]) >> tell(1)

      for {
        guess   <- askGuess
        nextMsg <- checkGuess(guess)
        _       <- proceed(nextMsg)
      } yield Option.when(guess == q)(())

    tryGuess.untilDefinedM.runL(q, "I have my number. What's your guess? ")

  def repeatSession(using s: Std[F], r: Random[F]): F[Option[String]] = 
    val askDefined = (msg: String) => [T] => (f: String => Option[T]) => s.ask(msg).map(f).untilDefinedM
    for {
      level    <- askDefined("Pick a difficulty level (1, 2, or 3):")(Level(_))
      question <- r.nextNat(level.max)
      count    <- session(question)
      _        <- s.println(s"You got it in $count guesses!")
      continue <- askDefined("Play again? ")(_.some.filter(s => s == "y" || s == "n"))
    } yield continue.some.filter(_ == "n")

  def exec(using s: Std[F], r: Random[F]): F[Unit] =
       s.println("Let's play Guess the Number.")
    >> repeatSession.untilDefinedM
    >> s.println("Goodbye!")

object Solution01 extends IOApp.Simple, Solution01[IO]:
  enum Level(val max: Int):
    case L1 extends Level(10)
    case L2 extends Level(100)
    case L3 extends Level(1000)
  object Level:
    def apply(s: String): Option[Level] = Try(Level.valueOf(s"L$s")).toOption

  trait Random[F[_]]:
    def nextNat(max: Int): F[Int]

  given Random[IO] with
    def nextNat(max: Int): IO[Int] = IO.pure(scala.util.Random.nextInt(max) + 1)
  def run: IO[Unit] = exec