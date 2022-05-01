package exercises.ex07

import scala.util.Try
import cats.MonadError
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.effect.IO
import cats.effect.IOApp
import exercises.common.{ Std, given }

object Solution01 extends IOApp.Simple :
  val Ratio = 0.09290304

  def exec[F[_]](using s: Std[F], m: MonadError[F, Throwable]): F[Unit] =
    val askLength = s.ask("What is the length of the room in feet? ")
    val askWidth  = s.ask("What is the width of the room in feet? ")
    val convert   = (s: String) => m.fromTry(Try(s.toInt))

    val buildOutput = (l: Int, w: Int) =>
      val af = l * w
      val am = af * Ratio
      f"""You entered dimensions of $l feet by $w feet.
         |The area is
         |$af square feet
         |$am%.3f square meters""".stripMargin

    val result = for {
      l <- askLength >>= convert
      w <- askWidth  >>= convert
    } yield buildOutput(l, w)

    result >>= s.println

  def run: IO[Unit] = exec[IO]