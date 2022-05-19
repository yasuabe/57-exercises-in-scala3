package exercises.ex40

import scala.math.max
import java.time.LocalDate
import cats.MonadThrow
import cats.Monoid
import cats.Functor
import cats.Order
import cats.syntax.flatMap.*
import cats.syntax.option.*
import cats.syntax.compose.*
import cats.syntax.functor.*
import cats.syntax.foldable.*
import cats.syntax.traverse.*
import cats.effect.{ IO, IOApp }
import exercises.common.{ Std, given }
import Solution01.*

// exercise 40: Filtering Records
trait Random[F[_]]:
  def nextNonNega(max: Int): F[Int]

trait Solution01[F[_]: Std: MonadThrow]:
  import Column.*
  import Solution01.{ given }

  def exec(using S: Std[F]): F[Unit] =
    given Monoid[(Int, Int, Int)] with
      def empty = (4, 8, 15)
      def combine(x: Triple[Int], y: Triple[Int]): Triple[Int] = (x, y) match 
        case ((a1, a2, a3), (b1, b2, b3)) => (max(a1, b1), max(a2, b2), max(a3, b3))

    for {
      str        <- S.ask("Enter a search string: ")
      _          <- S.println("Results:")
      rows       =  Database.filter(_ matches str).sorted.map(_.toStrings)
      (u, v, w)  =  rows.foldMap((_: Triple[String]).map(_.length))
      fromTriple =  (m: Triple[String]) => s"%-${u}s   | %-${v}s        | %-${w}s".format(m._1, m._2, m._3)
      _          <- S.println(fromTriple("Name ", "Position", "Separation Date"))
      _          <- S.println(s"${"-" * (u + 3)}|${"-" * (v + 9)}|${"-" * (w + 1)}")
      _          <- rows.traverse(fromTriple >>> S.println)
    } yield ()

object Solution01 extends IOApp.Simple, Solution01[IO]:
  enum Column:
    case NameCol, PositionCol, SeparationCol
  import Column.*

  type Triple[T] = (T, T, T)
  given ft: Functor[Triple] with
    def map[A, B](t: Triple[A])(f: A => B): Triple[B] = (f(t._1),f(t._2), f(t._3))

  opaque type Name           = (String, String)
  opaque type Position       = String
  opaque type SeparationDate = Option[LocalDate]

  extension (n: Name) 
    def length:              Int     = n._1.length + n._2.length + 1
    def contains(s: String): Boolean = n._1.contains(s) || n._2.contains(s)

  object SeparationDate:
    def apply(s: String): SeparationDate = Option.when(s.nonEmpty)(LocalDate.parse(s))

  type Value  = Name| Position| SeparationDate
  type Record = Map[Column, Value]

  extension (t: (String, String, String, String)) def toMap: Record = t match
    case (fn, ln, p, sd) => Map(
      NameCol       -> (fn, ln),
      PositionCol   -> p,
      SeparationCol -> SeparationDate(sd))

  extension (v: Value)
    def show = v match
      case n: Name           => s"${n._1} ${n._2}"
      case p: Position       => p
      case s: SeparationDate => s.fold("")(_.toString)

  extension (r: Record)
    def name:       Name           = r(NameCol).asInstanceOf[Name]
    def position:   Position       = r(PositionCol).asInstanceOf[Position]
    def separation: SeparationDate = r(SeparationCol).asInstanceOf[SeparationDate]

    infix def matches(s: String): Boolean = r.name.contains(s)
    def toStrings: Triple[String] = ((NameCol, PositionCol, SeparationCol): Triple[Column]).map(r(_).show)

  given Ordering[Record] =
    Ordering.by((_: Record).name).orElseBy(_.position).orElseBy(_.separation)

  val Database: List[Record] = List( // Constraint: use array of map
    // First Name |  Last Name  |  Position             | Separation date
    ("John"       , "Johnson"   , "Manager"             , "2016-12-31").toMap,
    ("Tou"        , "Xiong"     , "Software Engineer"   , "2016-10-05").toMap,
    ("Michaela"   , "Michaelson", "District Manager"    , "2015-12-19").toMap,
    ("Jake"       , "Jacobson"  , "Programmer"          , ""          ).toMap,
    ("Jacquelyn"  , "Jackson"   , "DBA"                 , ""          ).toMap,
    ("Sally"      , "Weber"     , "Web Developer Weber" , "2015-12-18").toMap)

  val ReplyList = Vector("Yes", "No", "Maybe", "Ask again later")

  given Random[IO] with
    def nextNonNega(max: Int): IO[Int] = IO.pure(scala.util.Random.nextInt(max))
  def run: IO[Unit] = exec
