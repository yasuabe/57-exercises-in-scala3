package exercises.ex53

import cats.MonadThrow
import cats.syntax.flatMap._
import cats.effect.{ IO, IOApp }
import io.circe.generic.auto.*
import org.http4s.ember.server.EmberServerBuilder
import org.http4s.dsl.io.*
import org.http4s.circe.jsonOf
import org.http4s.{ HttpRoutes, EntityDecoder }
import com.comcast.ip4s.*
import dev.profunktor.redis4cats.Redis
import dev.profunktor.redis4cats.effect.Log.Stdout._

// exercise 53: TODO List
type Todo = String
case class Task(id: Int, todo: Todo)

trait KVStore[F[_]]:
  def list: F[List[Task]]
  def add(todo: Todo): F[Task]
  def delete(id: Int): F[Unit]

trait Solution01[F[_]: MonadThrow]:
  import Solution01.*

  def list(using K: KVStore[F]): F[List[Task]] = K.list

  def addTask(t: Todo)(using M: MonadThrow[F], K: KVStore[F]): F[Task] =
    M.raiseWhen(t.isEmpty)(Exception("bad request")) >> K.add(t)

  def delete(id: Int)(using K: KVStore[F]): F[Unit] = K.delete(id)

object Solution01 extends IOApp.Simple, Solution01[IO]:
  import org.http4s.circe.CirceEntityEncoder.*
  import org.http4s.circe.CirceEntityDecoder.*

  def todoService(using KVStore[IO]) = HttpRoutes.of[IO] {
    case GET  -> Root / "todoList" => list >>= (Ok(_))

    case req @ POST -> Root / "todoList" => 
      (req.as[Todo] >>= addTask).attempt.flatMap(_.fold(_ => NotAcceptable(), Ok(_)))

    case DELETE -> Root / "todoList" / IntVar(id) => delete(id) >> Ok()
  }.orNotFound

  def run: IO[Unit] =
    Redis[IO].utf8("redis://localhost").use { redis =>
      given KVStore[IO] with
        extension (k: String) def toId = k.drop(5).toInt
        def list: IO[List[Task]] =  for {
          keys    <- redis.keys("todo:*")
          entries <- redis.mGet(keys.toSet)
        } yield entries.map { (k, v) => Task(k.toId, v) }.toList

        def add(todo: Todo): IO[Task] = 
          val mkMaxId = (_: List[String]).map(_.toId).max + 1
          for {
            n <- redis.keys("todo:*").map(mkMaxId)
            _ <- redis.set(s"todo:$n", todo)
          } yield Task(n.toInt, todo)

        def delete(id: Int): IO[Unit] = redis.del(s"todo:$id").void

      EmberServerBuilder
        .default[IO]
        .withHost(ipv4"0.0.0.0")
        .withPort(port"8080")
        .withHttpApp(todoService)
        .build
        .use(_ => IO.never)
        .void
    }