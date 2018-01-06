package ipoemi.scalawithcats.exercise

import org.scalatest.{Matchers, WordSpec}

class Chapter04Spec extends WordSpec with Matchers {

  "4.8.3 Exercise: Hacking on Readers" should {
    import `4.8.3`._

    val users = Map(
      1 -> "dade",
      2 -> "kate",
      3 -> "margo"
    )

    val passwords = Map(
      "dade" -> "zerocool",
      "kate" -> "acidburn",
      "margo" -> "secret"
    )

    val db = Db(users, passwords)

    "return true for the following" in {
      checkLogin(1, "zerocool").run(db) shouldBe true
    }

    "return false for the following" in {
      checkLogin(4, "davinci").run(db) shouldBe false
    }

  }

}
