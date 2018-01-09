package ipoemi.scalawithcats.exercise

import java.io.ByteArrayOutputStream

import scala.language.postfixOps

import org.scalatest.{Matchers, WordSpec}

class Chapter04Spec extends WordSpec with Matchers {


  "4.1.2 Exercise: Gettng Func-y" should {
    import `4.1.2`._

    implicit val listMonad = new Monad[List] {
      def pure[A](a: A): List[A] = List(a)
      def flatMap[A, B](value: List[A])(func: A => List[B]): List[B] = value.flatMap(func)
    }

    "return List(2, 4, 6) for the following" in {
      implicitly[Monad[List]].map(List(1, 2, 3))(_ * 2) shouldBe List(2, 4, 6)
    }
  }


  "4.3.1 Exercise: Monadic Secret Identities" should {
    import cats.Id
    import `4.1.2`._
    import `4.3.1`.idMonad

    "return 2 for the following" in {
      implicitly[Monad[Id]].map(1)(_ * 2) shouldBe 2
    }

  }

  "4.6.5 Exercise: Safer Folding using Eval" should {
    import cats.Eval
    import `4.6.5`.foldRight

    def factorial(n: BigInt): Eval[BigInt] =
      if (n == 1) {
        Eval.now(n)
      } else {
        Eval.defer(factorial(n - 1).map(_ * n))
      }

    "must equal the following" in {
      foldRight((1 to 50000).toList, BigInt(1))(_ * _) shouldBe factorial(50000).value
    }

  }

  "4.7.3 Exercise: Show Your Working" should {
    "print one at a time" in {
      import `4.7.3`._
      import scala.concurrent._
      import scala.concurrent.ExecutionContext.Implicits.global
      import scala.concurrent.duration._

      val stream1 = new ByteArrayOutputStream()
      val stream2 = new ByteArrayOutputStream()
      Console.withOut(stream1) {
        factorial(3)
        factorial(5)
      }
      Console.withOut(stream2) {
        Await.result(Future.sequence(Seq(
          Future(factorial2(3)),
          Future(factorial2(5))
        )), 5 seconds)
      }
      stream1.toString shouldBe stream2.toString
    }
  }


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

  "4.9.3 Exercise: Post-Order Calculator" should {
    import `4.9.3`._

    "return 42 for the following" in {
      evalOne("42").runA(Nil).value shouldBe 42
    }

    "return 3 for the following" in {
      val program = for {
        _ <- evalOne("1")
        _ <- evalOne("2")
        ans <- evalOne("+")
      } yield ans

      program.runA(Nil).value shouldBe 3
    }

    "return 9 for the following" in {
      val program = evalAll(List("1", "2", "+", "3", "*"))
      program.runA(Nil).value shouldBe 9
    }

    "return 21 for the following" in {
      val program = for {
        _ <- evalAll(List("1", "2", "+"))
        _ <- evalAll(List("3", "4", "+"))
        ans <- evalOne("*")
      } yield ans

      program.runA(Nil).value shouldBe 21
    }

  }

  "4.10.1 Exercise: Branching out Further with Monads" should {
    import `4.10.1`._
    import cats._, cats.implicits._

    "return branch(branch(leaf(2), leaf(4)), branch(leaf(3), leaf(6))) for the following" in {
      branch(leaf(2), leaf(3)).flatMap(x => branch(leaf(x), leaf(x * 2))) shouldBe
        branch(branch(leaf(2), leaf(4)), branch(leaf(3), leaf(6)))
    }

    "return branch(branch(leaf(99),leaf(101)),branch(leaf(199),leaf(201)))" in {
      branch(leaf(100), leaf(200)).flatMap(x => branch(leaf(x - 1), leaf(x + 1))) shouldBe
        branch(branch(leaf(99), leaf(101)), branch(leaf(199), leaf(201)))
    }

    """
      |return
      |  Branch(
      |    Branch(
      |      Branch(Leaf(89),Leaf(91)),
      |      Branch(Leaf(109),Leaf(111))),
      |    Branch(
      |      Branch(Leaf(189),Leaf(191)),
      |      Branch(Leaf(209),Leaf(211))))
    """.stripMargin in {
      var result = for {
        a <- branch(leaf(100), leaf(200))
        b <- branch(leaf(a - 10), leaf(a + 10))
        c <- branch(leaf(b - 1), leaf(b + 1))
      } yield c

      result shouldBe
        branch(branch(branch(leaf(89), leaf(91)),
          branch(leaf(109), leaf(111))), branch(branch(leaf(189), leaf(191)),
          branch(leaf(209), leaf(211))))
    }
  }
}
