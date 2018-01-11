package ipoemi.scalawithcats.exercise

import org.scalatest.{Matchers, WordSpec}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent._
import scala.concurrent.duration._

class Chapter07Spec extends WordSpec with Matchers {

  "7.1.3 Exercise: Scaf-fold-ing Other Methods" should {
    import `7.1.3`._
    import cats.instances.int._
    import cats.instances.stream._

    "return map result for the following" in {
      var xs = List(1, 2, 3)
      var fn = (x: Int) => x + 1
      map(xs)(fn) shouldBe xs.map(fn)
    }

    "return flatMap result for the following" in {
      var xs = List(1, 2, 3)
      var fn = (x: Int) => List.fill(x)(x)
      flatMap(xs)(fn) shouldBe xs.flatMap(fn)
    }

    "return filter result for the following" in {
      var xs = List(1, 2, 3)
      var fn = (x: Int) => x > 2
      filter(xs)(fn) shouldBe xs.filter(fn)
    }

    "return sum result for the following" in {
      var xs = List(1, 2, 3)
      sum(xs) shouldBe xs.sum
    }

  }

  "7.2.2.1 Exercise: Traversing with Vectors" should {
    import `7.2.2.1`._
    import cats.Applicative
    import cats.syntax.applicative._
    import cats.syntax.apply._
    import cats.instances.vector._ // for Applicative

    "return Vector(List(1, 3), List(1, 4), List(2, 3), List(2, 4)) for the following" in {
      listSequence(List(Vector(1, 2), Vector(3, 4))) shouldBe Vector(List(1, 3), List(1, 4), List(2, 3), List(2, 4))
    }

    """return
        Vector(
          List(1, 3, 5), List(1, 3, 6),
          List(1, 4, 5), List(1, 4, 6),
          List(2, 3, 5), List(2, 3, 6),
          List(2, 4, 5), List(2, 4, 6)
        ) for the following""" in {
      listSequence(List(Vector(1, 2), Vector(3, 4), Vector(5, 6))) shouldBe
        Vector(
          List(1, 3, 5), List(1, 3, 6),
          List(1, 4, 5), List(1, 4, 6),
          List(2, 3, 5), List(2, 3, 6),
          List(2, 4, 5), List(2, 4, 6)
        )
    }
  }

  "7.2.2.2 Exercise: Traversing with Options" should {
    import `7.2.2.2`._

    "return Some(List(2, 4, 6)) for the following" in {
      import cats._, cats.data._, cats.implicits._

      process(List(2, 4, 6)) shouldBe Some(List(2, 4, 6))
    }

    "return None for the following" in {
      import cats._, cats.data._, cats.implicits._

      process(List(1, 2, 3)) shouldBe None
    }

  }

  "7.2.2.3 Exercise: Traversing with Validated" should {
    import `7.2.2.3`._


    "return valid(List(2, 4, 6) for the following" in {
      import cats._, cats.data._, cats.implicits._

      process(List(2, 4, 6)) shouldBe Validated.valid(List(2, 4, 6))
    }

    "return invalid(List(\"1 is not even\", \"3 is not even\") for the following" in {
      import cats._, cats.data._, cats.implicits._

      process(List(1, 2, 3)) shouldBe Validated.invalid(List("1 is not even", "3 is not even"))
    }

  }

}
