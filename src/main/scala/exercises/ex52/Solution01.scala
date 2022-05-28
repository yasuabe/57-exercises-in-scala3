package exercises.ex52

import java.time.LocalDateTime
import java.time.Month
import java.time.format.DateTimeFormatter 
import cats.MonadThrow
import cats.ApplicativeThrow
import cats.syntax.applicative._
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.effect.{ IO, IOApp }
import org.http4s.client.*
import org.http4s.client.JavaNetClientBuilder
import io.circe.parser.*
import io.circe.generic.auto.*
import io.circe.Decoder
import io.circe.HCursor
import exercises.common.{ Std, given }

// exercise 52: Creating Your Own Time Service
case class Response(currentTime: LocalDateTime):
  export currentTime.{ getHour, getMinute, getSecond, getMonth, getDayOfMonth, getYear }

trait CTClient[F[_]]:
  def retrieve(url: String): F[String]

trait Environment[F[_]]:
  def url: F[String]

trait Solution01[F[_]: MonadThrow]:
  val format = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss")

  given Decoder[LocalDateTime] with
    def apply(c: HCursor): Decoder.Result[LocalDateTime] =
      c.as[String].map(LocalDateTime.parse(_, format))

  extension (m: Month) def show: String = m.name.head + m.name.tail.toLowerCase

  val render: Response => String = p =>
    "The current time is %02d:%02d:%02d UTC %s %d %d.".format(
      p.getHour(),
      p.getMinute(),
      p.getSecond(),
      p.getMonth().show,
      p.getDayOfMonth(),
      p.getYear()
    )

  def exec(using 
    S: Std[F],
    A: CTClient[F],
    E: Environment[F],
    T: ApplicativeThrow[F]): F[Unit] = for {
    url  <- E.url
    json <- A.retrieve(url)
    res  <- T.fromEither(decode[Response](json))
    _    <- S.println(render(res))
  } yield ()

object Solution01 extends IOApp.Simple, Solution01[IO]:
  val httpClient: Client[IO] = JavaNetClientBuilder[IO].create

  given CTClient[IO] with
    def retrieve(url: String): IO[String] = httpClient.expect[String](url)

  given Environment[IO] with
    def url: IO[String] = "http://localhost:8080/currentTime".pure[IO]

  def run: IO[Unit] = exec