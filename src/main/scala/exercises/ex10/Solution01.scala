package exercises.ex10

import cats.MonadError
import cats.Monoid
import cats.syntax.flatMap.*
import cats.syntax.foldable.*
import cats.syntax.traverse.*
import cats.syntax.functor.*
import cats.syntax.apply.*
import cats.effect.IO
import cats.effect.IOApp
import exercises.common.{ Std, given }
import scala.util.chaining.*

// exercise 10: Self-Checkout
trait Solution01[F[_]]:
  import Solution01.*

  def exec(using s: Std[F], m: MonadError[F, Throwable]): F[Unit] =
    val askPriceAndQty = (n: Int) =>
      def f[T](prompt: String, conversion: String => T) =
        s.ask(prompt) >>= (in => m.catchNonFatal(conversion(in)))
      m.product(
        f(s"Enter the price of item $n: "   , s => Money(s.toInt)),
        f(s"Enter the quantity of item $n: ", _.toInt            ))

    val buildResult = (pairs: LazyList[(Money, Int)]) =>
      val sub   = pairs.foldMap(_ * _)
      val tax   = sub * TaxRate
      val total = sub + tax
      s"""Subtotal: ${sub.show}
        |Tax: ${tax.show}
        |Total: ${total.show}""".stripMargin

    LazyList
      .continually(askPriceAndQty)
      .zip(1 to 3)
      .traverse(_(_))
      .map(buildResult)
      .flatMap(s.println)

object Solution01 extends IOApp.Simple, Solution01[IO] :
  val TaxRate = 0.055

  opaque type Money = Double

  private val doubleMonoid = summon[Monoid[Double]]
  object Money:
    def apply(d: Double): Money = d
    given a: Monoid[Money] = doubleMonoid

  extension (m: Money)
    def +(n: Money): Money  = m + n
    def *(n: Double): Money = m * n
    def show: String = f"$$$m%.2f"

  def run: IO[Unit] = exec
