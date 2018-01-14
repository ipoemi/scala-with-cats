package ipoemi.scalawithcats.exercise

import org.scalatest.{Matchers, WordSpec}

class Chapter09Spec extends WordSpec with Matchers {

  "9.2 Implementing foldMap" should {
    import `9.2`._

    "return 6 for the following" in {
      import cats.instances.int._

      foldMap(Vector(1, 2, 3))(identity) shouldBe 6
    }

    "return \"1! 2! 3! \" for the following" in {
      import cats.instances.string._

      foldMap(Vector(1, 2, 3))(_.toString + "! ") shouldBe "1! 2! 3! "
    }

    "return \"HELLO WORLD!\" for the following" in {
      import cats.instances.string._

      foldMap("Hello World!".toVector)(_.toString.toUpperCase) shouldBe "HELLO WORLD!"
    }

  }

  "9.3.3 Implementing foldMap" should {
    import scala.concurrent._
    import scala.concurrent.ExecutionContext.Implicits.global
    import scala.concurrent.duration._

    import cats.Monoid
    import cats.instances.int._

    import `9.3.3`._


    "return 1784293664 for the following" in {
      val result = parallelFoldMap((1 to 1000000).toVector)(identity)
      Await.result(result, 1 second) shouldBe 1784293664
    }

  }

  "9.3.4 parallelFoldMap with more Cats" should {
    import scala.concurrent._
    import scala.concurrent.ExecutionContext.Implicits.global
    import scala.concurrent.duration._

    import cats.Monoid
    import cats.instances.int._

    import `9.3.3`._


    "return 1784293664 for the following" in {
      val result = parallelFoldMap((1 to 1000).toVector)(_ * 1000)
      Await.result(result, 1 second) shouldBe 500500000
    }

  }

}
