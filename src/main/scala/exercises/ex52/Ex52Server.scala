package exercises.ex52

import java.time.LocalDateTime
import java.time.ZoneOffset
import java.time.format.DateTimeFormatter 
import scala.concurrent.duration.FiniteDuration
import cats.Functor
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.effect.{ IO, IOApp }
import cats.effect.Clock
import io.circe.generic.auto.*
import io.circe.syntax.*
import io.circe.Json
import io.circe.Encoder
import org.http4s.HttpRoutes
import org.http4s.dsl.io.*
import org.http4s.circe.*
import org.http4s.ember.server.*
import com.comcast.ip4s.*

// exercise 52: Creating Your Own Time Service
trait Ex52Server[F[_]]:
  case class Response(currentTime: FiniteDuration)

  given Conversion[FiniteDuration, LocalDateTime] with
    def apply(fd: FiniteDuration): LocalDateTime =
      LocalDateTime.ofEpochSecond(fd.toSeconds, 0, ZoneOffset.UTC)

  given Encoder[FiniteDuration] with
    def apply(fd: FiniteDuration): Json = 
      DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss").format(fd).asJson

  def currentTime(using M: Functor[F], C: Clock[F]): F[Json] = 
    C.realTime.map(Response(_).asJson)

object Ex52Server extends IOApp.Simple, Ex52Server[IO]:
  val helloWorldService = HttpRoutes.of[IO] {
    case GET -> Root / "currentTime" => currentTime >>= (Ok(_))
  }.orNotFound

  def run: IO[Unit] =
    EmberServerBuilder
      .default[IO]
      .withHost(ipv4"0.0.0.0")
      .withPort(port"8080")
      .withHttpApp(helloWorldService)
      .build
      .use(_ => IO.never)
      .void