package exercises.ex27

import cats.MonadThrow
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.syntax.traverse.*
import cats.syntax.either.*
import cats.syntax.apply.*
import cats.syntax.compose.*
import cats.data.ValidatedNec
import cats.effect.IO
import cats.effect.IOApp
import exercises.common.{ Std, given }
import eu.timepit.refined.refineV
import eu.timepit.refined.string.MatchesRegex
import eu.timepit.refined.api.Refined

// exercise 27: Validating Inputs
object InputTypes:
  private type NamePartP = MatchesRegex[".{2,}"]
  opaque  type NamePart  = String Refined NamePartP
  object NamePart:
    def apply(s: String): Either[String, NamePart] = refineV[NamePartP](s)

  private type ZipP = MatchesRegex["[0-9]+"]
  opaque  type Zip  = String Refined ZipP 
  object Zip:
    def apply(s: String): Either[String, Zip] = refineV[ZipP](s)

  private type EmployeeIdP = MatchesRegex["[A-Z]{2}-[0-9]{4}"]
  opaque  type EmployeeId  = String Refined EmployeeIdP
  object EmployeeId:
    def apply(s: String): Either[String, EmployeeId] = refineV[EmployeeIdP](s)

trait Solution01[F[_]: MonadThrow]:
  import InputTypes.*

  type Inputs = (NamePart, NamePart, Zip, EmployeeId)

  def nameRule(f: String): String => Either[String, NamePart] = s =>
    Either.cond(s.nonEmpty, s, s"The $f must be filled in.")
    >>= (s => NamePart(s).leftMap(_ => s""" "$s" is not a valid $f. It is too short."""))

  def zipRule(f: String): String => Either[String, Zip] = s =>
    Zip(s).leftMap(_ => s"The $f must be numeric.")

  def employeeIdRule(f: String): String => Either[String, EmployeeId] = s =>
    EmployeeId(s).leftMap(_ => f"$s is not a valid $f.")

  val validate = [T] => (f: String => String => Either[String, T], l:String, in: String) =>
    f(l)(in).toValidatedNec

  def exec(using s: Std[F]): F[Unit] =
    val input = (
      "Enter the first name: ",
      "Enter the last name: ",
      "Enter the ZIP code: ",
      "Enter an employee ID: "
    ).map[[X] =>> F[String]]([T] => (msg: T) => s.ask(s"$msg"))
     .mapN((_, _, _, _))

    val validateInputs: ((String, String, String, String)) => ValidatedNec[String, Inputs] =
      (firstName, lastName, zipCode, empId) => 
        //         rule          | field       | input
        ( validate(nameRule      , "first name", firstName),
          validate(nameRule      , "last name" , lastName),
          validate(zipRule       , "ZIP code"  , zipCode),
          validate(employeeIdRule, "ID"        , empId),
        ).mapN((_, _, _, _))

    val report = (v: ValidatedNec[String, Inputs]) =>
      v.fold[F[Unit]](
        _.traverse(s.println).void,
        _ => s.println("There are no errors are found"))

    input >>= (validateInputs >>> report)

object Solution01 extends IOApp.Simple, Solution01[IO]:
  def run: IO[Unit] = exec