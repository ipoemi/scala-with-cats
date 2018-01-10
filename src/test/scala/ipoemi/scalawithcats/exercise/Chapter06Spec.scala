package ipoemi.scalawithcats.exercise

import java.math.BigInteger

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

}
