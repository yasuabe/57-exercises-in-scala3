package exercises.ex20

import scala.util.chaining.*
import cats.MonadThrow
import cats.syntax.flatMap.*
import cats.syntax.option.*
import cats.syntax.compose.*
import cats.syntax.traverse.*
import cats.syntax.functor.*
import cats.syntax.flatMap.*
import cats.syntax.applicative.*
import cats.syntax.applicativeError.*
import cats.effect.IO
import cats.effect.IOApp
import exercises.common.{ Std, given }

// exercise 20: Multistate Sales Tax Calculator
trait Solution01[F[_]]:
  import Solution01.*

  def exec(using s: Std[F], m: MonadThrow[F]): F[Unit] =
    val askOrder: F[Double] =
      s.ask("What is the order amount? ") >>= (s => m.catchNonFatal(s.toDouble))

    val askState : F[String] = s.ask("What state do you live in?")

    val askCounty: String => F[Option[String]] = state =>
      Option.when(state == "Wisconsin")(s.ask("What county do you live in?")).sequence

    val taxRatio: (String, Option[String]) =>  Option[Double] = (_, _) match
      case (_          , Some("Eau Claire")) => 0.055.some
      case (_          , Some("Dunn"      )) => 0.054.some
      case ("Wisconsin", _                 ) => 0.05.some
      case ("Illinois" , _                 ) => 0.08.some
      case _                                 => none

    extension [A](a: A)
      def applyOption[B](ob: Option[B], f: (A, B) => A): A = ob.fold(a)(b => f(a, b))

    val mkReport: (Double, Option[Double]) => List[String] = (order, ratio) =>
      ratio.map(_ * order).fold(
        f"The total is $$$order%.2f.".pure[List]
      ) { tax =>
        f"The tax is $$$tax%.2f."
        :: f"The total is $$${order + tax}%.2f."
        :: Nil
      }

    for
      order  <- askOrder
      state  <- askState
      county <- askCounty(state)
      ratio  <- taxRatio(state, county).pure
      _      <- mkReport(order, ratio).traverse(s.println)
    yield ()

object Solution01 extends IOApp.Simple, Solution01[IO]:

  def run: IO[Unit] = exec
