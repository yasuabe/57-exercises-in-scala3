package exercises.ex33

// import scala.util.Try
import cats.MonadThrow
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.effect.{ IO, IOApp }
import exercises.common.{ Std, given }
import Solution01.*

// exercise 33: Magic 8 Ball
trait Solution01[F[_]: Std: MonadThrow]:
  def exec(using S: Std[F], R: Random[F]): F[Unit] = for {
    _     <- S.ask("What's your question?")
    reply <- R.nextNonNega(4).map(ReplyList(_))
    _     <- S.println(s"$reply.")
  } yield ()

object Solution01 extends IOApp.Simple, Solution01[IO]:
  val ReplyList = Vector("Yes", "No", "Maybe", "Ask again later")
  trait Random[F[_]]:
    def nextNonNega(max: Int): F[Int]

  given Random[IO] with
    def nextNonNega(max: Int): IO[Int] = IO.pure(scala.util.Random.nextInt(max))
  def run: IO[Unit] = exec