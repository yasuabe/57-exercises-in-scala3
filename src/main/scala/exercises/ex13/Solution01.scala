package exercises.ex13

import cats.MonadError
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.effect.IO
import cats.effect.IOApp
import exercises.common.{ Std, given }

// exercise 13: Determining Compound Interest
trait Solution01[F[_]]:
  import Solution01.*
  def exec(using s: Std[F], m: MonadError[F, Throwable]): F[Unit] =
    val gatherInputs =
      val enter = [T] => (name: String, convert: String => T)  =>
        s.ask(s"Enter the $name: ") >>= (s => m.catchNonFatal(convert(s)))
      for {
        p <- enter("principal amount"   , _.toInt)
        r <- enter("rate"               , _.toDouble)
        t <- enter("the number of years", _.toInt)
        n <- enter("the number of times the interest is compounded per year", _.toInt)
      } yield (p, r, t, n)

    val buildResult: ((Int, Double, Int, Int)) => String = (p, r, t, n) =>
      val a = (p * (1 + r / 100.0 / n).pow(t * n)).roundAt2
      s"""$$$p was invested at $r% for $t years
         |compounded $n times per years is $$$a.""".stripMargin

    gatherInputs.map(buildResult) >>= s.println

object Solution01 extends IOApp.Simple, Solution01[IO] :
  extension (d: Double)
    def pow(exponent: Double): Double = scala.math.pow(d, exponent)
    def roundAt2: Double = (d * 100).round / 100.0

  def run: IO[Unit] = exec
