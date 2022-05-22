package exercises.ex41

import cats.effect.{ IO, IOApp }
import fs2.{ Stream, Pipe, text }

// exercise 41: Name Sorter
trait Solution01[F[_]]:

  def exec: Pipe[F, String, String] = (_: Stream[F, String]).flatMap { t  =>
    val lines = t.linesIterator.toList
    Stream.emits(s"Total of ${lines.length} names" ::
                 "-----------------"               ::
                 lines.sorted).intersperse("\n")
  }

object Solution01 extends IOApp.Simple, Solution01[IO]:
  import fs2.io.file.{ Files, Path }

  def run: IO[Unit] = Files[IO]
    .readAll(Path("testdata/in/ex41names.txt"))
    .through(text.utf8.decode)
    .through(exec)
    .through(text.utf8.encode)
    .through(Files[IO].writeAll(Path("testdata/out/ex41sorted.txt")))
    .compile
    .drain
