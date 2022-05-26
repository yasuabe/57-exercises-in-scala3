package exercises.ex47

import scala.math.max
import cats.MonadThrow
import cats.ApplicativeThrow
import cats.syntax.applicative._
import cats.syntax.flatMap._
import cats.syntax.traverse._
import cats.syntax.functor._
import cats.effect.{ IO, IOApp }
import org.http4s.client.*
import org.http4s.ember.client.*
import org.http4s.client.JavaNetClientBuilder
import io.circe.Parser
import io.circe.parser.*
import io.circe.generic.auto.*
import io.circe.syntax.*
import io.circe.refined.*
import eu.timepit.refined.types.numeric.PosInt
import eu.timepit.refined.types.string.NonEmptyString
import eu.timepit.refined.auto.autoUnwrap
import exercises.common.{ Std, given }

// exercise 47: Who’s in Space?
type Craft = NonEmptyString 
type Name  = NonEmptyString 

case class Person(craft: Craft, name: Name)
case class Space(people: List[Person], message: String, number: PosInt)

trait AstrosService[F[_]]:
  def retrieve(url: String): F[String]

trait Environment[F[_]]:
  def url: F[String]

trait Solution01[F[_]: MonadThrow]:
  val render: Space => List[String] = p =>
    val (maxName, maxCraft) = p.people.foldLeft(("Name".length, "Craft".length)) {
      case ((accN, accC), Person(c, n)) => (max(accN, n.length), max(accC, c.length))
    }
    val showRow: (String, String) => String = s"%-${maxName}s   | %s".format(_, _)
    val showPerson: Person        => String = p => showRow(p.name, p.craft)

    s"There are ${p.people.length} people in space right now:"
    :: ""
    :: showRow("Name", "Craft")
    :: ("-" * (maxName + 2)) + "-|-" + ("-" * maxCraft)
    :: p.people.map(showPerson)

  def exec(using 
    S: Std[F],
    A: AstrosService[F],
    E: Environment[F],
    T: ApplicativeThrow[F]): F[Unit] = for {
    url    <- E.url
    json   <- A.retrieve(url)
    people <- T.fromEither(decode[Space](json))
    _      <- render(people).traverse(S.println)
  } yield ()

object Solution01 extends IOApp.Simple, Solution01[IO]:
  val httpClient: Client[IO] = JavaNetClientBuilder[IO].create

  given AstrosService[IO] with
    def retrieve(url: String): IO[String] = httpClient.expect[String](url)

  given Environment[IO] with
    def url: IO[String] = "http://api.open-notify.org/astros.json".pure[IO]

  def run: IO[Unit] = exec