package ipoemi.scalawithcats.exercise

import java.math.BigInteger

import cats.data.NonEmptyVector
import org.scalatest.{Matchers, WordSpec}

import scala.concurrent._
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global

class Chapter06Spec extends WordSpec with Matchers {

  "6.3.1.1 Exercise: The Product of Monads" should {

    import `6.3.1.1`._
    import cats._, cats.implicits._

    "return Some((1, \"no\"))" in {
      product(1.some, "no".some) shouldBe Some((1, "no"))
    }

    "return List((1,3), (1,4), (2,3), (2,4))" in {
      product(List(1, 2), List(3, 4)) shouldBe List((1, 3), (1, 4), (2, 3), (2, 4))
    }

    "return ErrorOr[(Nothing, Nothing)] = Left(Vector(Error 1))" in {
      type ErrorOr[A] = Either[Vector[String], A]

      product[ErrorOr, Nothing, Nothing](Left(Vector("Error 1")), Left(Vector("Error 2"))) shouldBe
        Left(Vector("Error 1"))
    }

    "return (\"Hello\", 123)" in {
      Await.result(product(Future("Hello"), Future(123)), 1 second) shouldBe("Hello", 123)
    }
  }

  "6.4.4 Exercise: Form Validation" should {
    import `6.4.4`._
    import cats._, cats.data._, cats.implicits._

    "return Left(List(\"the name and age must be specified\")) for the following" in {
      readName(Map()) shouldBe Left(List("the name and age must be specified"))
      readAge(Map()) shouldBe Left(List("the name and age must be specified"))
    }

    "return Left(List(\"the name must not be blank\")) for the following" in {
      readName(Map("name" -> "")) shouldBe Left(List("the name must not be blank"))
    }

    "return Left(List(\"the age must be a valid non-negative integer\")) for the following" in {
      readAge(Map("age" -> "")) shouldBe Left(List("the age must be a valid non-negative integer"))
    }

    val user1Map = Map("name" -> "user1", "age" -> "37")
    "return Right(\"user1\") for the following readName" in {
      readName(user1Map) shouldBe Right(user1Map("name"))
    }

    "return Right(37) for the following" in {
      readAge(user1Map) shouldBe Right(37)
    }

    "return None for the following getValue" in {
      getValue("name")(Map()) shouldBe None
    }

    "return Some(user1) for the following getValue" in {
      getValue("name")(Map("name" -> "user1")) shouldBe Some("user1")
    }

    "return None for the following parseInt" in {
      parseInt("") shouldBe None

      parseInt("1a") shouldBe None
    }

    "return Some(37) for the following parseInt" in {
      parseInt("37") shouldBe Some(37)
    }

    "return true for the following nonBlank" in {
      nonBlank("user1") shouldBe true
    }

    "return false for the following nonBlank" in {
      nonBlank("") shouldBe false
    }

    "return false for the following nonNegative" in {
      nonNegative(-1) shouldBe false

      nonNegative(-100) shouldBe false
    }

    "return true for the following nonNegative" in {
      nonNegative(1) shouldBe true

      nonNegative(0) shouldBe true
    }

    "return Valid(User) for the following" in {
      var userMap = Map("name" -> "user1", "age" -> "37")
      readUser(userMap) shouldBe Validated.valid(User("user1", 37))
    }

    "return Invalid(List(\"the name and age must be specified\", \"the name and age must be specified\")) for the following" in {
      var userMap = Map[String, String]()
      readUser(userMap) shouldBe
        Validated.invalid(List("the name and age must be specified", "the name and age must be specified"))
    }
  }

}
