package exercises.ex12

import cats.MonadError
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.effect.IO
import cats.effect.IOApp
import exercises.common.{ Std, given }

// exercise 12: Computing Simple Interest
trait Solution01[F[_]]:
  def exec(using s: Std[F], m: MonadError[F, Throwable]): F[Unit] =
    val gatherInputs =
      val enter = [T] => (name: String, convert: String => T)  =>
        s.ask(s"Enter the $name: ") >>= (s => m.catchNonFatal(convert(s)))
      for {
        p <- enter("principal"          , _.toInt)
        r <- enter("rate of interest"   , _.toDouble)
        t <- enter("the number of years", _.toInt)
      } yield (p, r, t)

    val buildResult: ((Int, Double, Int)) => String = (p, r, t) =>
      val a = p * ((100 + r * t).toInt / 100.0) 
      s"""After $t years at $r%, the investment will
         |be worth $$$a.""".stripMargin

    gatherInputs.map(buildResult) >>= s.println

object Solution01 extends IOApp.Simple, Solution01[IO] :
  def run: IO[Unit] = exec
