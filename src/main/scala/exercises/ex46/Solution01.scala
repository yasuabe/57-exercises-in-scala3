package exercises.ex46

import cats.syntax.option._
import cats.effect.{ IO, IOApp }
import fs2.{ Stream, Pipe, text }

// exercise 46: Word Frequency Finder
trait Solution01[F[_]]:

  def exec: Pipe[F, String, String] = (_: Stream[F, String]).flatMap { text  =>
    val counts = text
      .split("\\s+")
      .foldLeft(Map.empty[String, Int]) { (acc, word) =>
        acc.updatedWith(word)(_.fold(1)(_ + 1).some)
      }
    val maxLen = counts.map(_._1.length).max
    val desc = summon[Ordering[Int]].reverse
    val lines = counts
      .toList
      .sortBy(_._2)(desc)
      .map { (word, count) =>
        s"$word: ${" " * (maxLen - word.length)}${"*" * count}"
      }
    Stream.emits(lines).intersperse("\n")
  }

object Solution01 extends IOApp.Simple, Solution01[IO]:
  import fs2.io.file.{ Files, Path }

  def run: IO[Unit] = Files[IO]
    .readAll(Path("testdata/in/ex46words.txt"))
    .through(text.utf8.decode)
    .through(exec)
    .through(text.utf8.encode)
    .through(Files[IO].writeAll(Path("testdata/out/ex46result.txt")))
    .compile
    .drain
