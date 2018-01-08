package ipoemi.scalawithcats.exercise

import org.scalatest.{Matchers, NonImplicitAssertions, WordSpec}

class Chapter03Spec extends WordSpec with Matchers with NonImplicitAssertions {

  "3.5.4 Exercise: Branching out with Functors" should {
    import `3.5.4`.{Tree, Branch, Leaf}
    import `3.5.4`.Tree._


    "must be equal Leaf(func(a) and Leaf(a).map(func)" in {
      import cats.syntax.functor._

      leaf("a").map(_ + 1) shouldBe Leaf("a1")
      leaf(2).map(_ * 2) shouldBe Leaf(4)
    }

    "must be equal Branch(Leaf(func(a)), Leaf(func(b))) and Branch(Leaf(a), Leaf(b)).map(func)" in {
      import cats.syntax.functor._

      branch(Leaf(2), Leaf(4)).map(_ * 2) shouldBe Branch(Leaf(4), Leaf(8))
      branch(Leaf("a"), Leaf("b")).map(_ + 2) shouldBe Branch(Leaf("a2"), Leaf("b2"))
    }

    "must be equal Nested Branch(..Branch(Leaf(func(a))..) and Branch(..Branch(Leaf(a))..).map(func)" in {
      import cats.syntax.functor._

      var branch1 = branch(Leaf(2), Leaf(3))
      var branch2 = branch(branch1, Leaf(4))
      var branch3 = branch(branch1, branch2)

      var func: Int => Int = (_ * 2)

      branch3.map(func) shouldBe
        branch(branch1.map(func), branch(branch1.map(func), leaf(4).map(func)))
    }

  }

  "3.6.1.1 Exercise: Showing off with Contramap" should {
    import `3.6.1.1`._

    """return "hello world" for the following""" in {
      format(Box("hello world")) shouldBe "\"hello world\""
    }

    "return yes for the following" in {
      format(Box(true)) shouldBe "yes"
    }
  }

}
