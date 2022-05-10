package exercises.ex24

import cats.Applicative
import cats.data.StateT
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.syntax.monad._
import munit.FunSuite
import exercises.common.Pair._
import exercises.common.InOut._
import exercises.common.{ Std, given }
import exercises.common.StdState
import StdState.{*, given }
import munit.ScalaCheckSuite
import org.scalacheck.Prop._
import org.scalacheck.{ Arbitrary, Gen }
import org.scalacheck.cats.implicits._

class Solution01Test extends ScalaCheckSuite:
  val sut = new Solution01[StdState]{}

  def act(ss: String*) = sut.exec.runS(InOut(ss*)).map(_.unapply)

  given Gen[String] = for {
    n <- Gen.chooseNum(0, 3)
    s <- summon[Applicative[Gen]].replicateA(n, Gen.choose('a', 'c'))
  } yield s.mkString

  property("isAnagram is true for two strings with same character set") {
    val f = (s: String) => s.toCharArray.groupBy(identity).mapValues(_.size).toSet
    forAll { (x: String, y: String) =>
      (f(x) == f(y)) == Solution01.isAnagram(x, y)
    }
  }
  test("tell the note and tone are anagrams") {
    // ARRANGE / ACT
    val Right((Nil, out)) = act("note", "tone")

    //ASSERT
    assertEquals(out, List(
      "Enter two strings and I'll tell you if they are anagrams:",
      "Enter the first string:",
      "Enter the second string:",
      """"note" and "tone" are anagrams."""))
  }
  test("tell the note and aaaa are not anagrams") {
    // ARRANGE / ACT
    val Right((Nil, _ :: _ :: _ :: out :: Nil)) = act("aaaa", "abaa")

    //ASSERT
    assertEquals(out, """"aaaa" and "abaa" are not anagrams.""")
  }
  test("reject the second string whose length is different from the first") {
    // ARRANGE / ACT
    val Right((Nil, _ :: _ :: out)) = act("note", "ton", "oten")

    //ASSERT
    assertEquals(out, List(
      "Enter the second string:",
      "Enter the second string:",
      """"note" and "oten" are anagrams."""))
  }