package ipoemi.scalawithcats.exercise

import java.io.ByteArrayOutputStream

import org.scalatest.{Matchers, WordSpec}

import scala.language.postfixOps

class Chapter05Spec extends WordSpec with Matchers {

  "5.4 Exercise: Monads: Transform and Roll Out" should {
    import `5.4`._

    "return Jazz and Bumblebee need a recharge. for the following" in {
      tacticalReport("Jazz", "Bumblebee") shouldBe "Jazz and Bumblebee need a recharge."
    }

    "return Bumblebee and Hot Rod are ready to roll out! for the following" in {
      tacticalReport("Bumblebee", "Hot Rod") shouldBe "Bumblebee and Hot Rod are ready to roll out!"
    }

    "return Comms error: Ironhide unreachable for the following" in {
      tacticalReport("Jazz", "Ironhide") shouldBe "Comms error: Ironhide unreachable"
    }
  }

}
