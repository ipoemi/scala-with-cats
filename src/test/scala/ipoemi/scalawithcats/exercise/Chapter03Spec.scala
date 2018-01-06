package ipoemi.scalawithcats.exercise

import org.scalatest.{Matchers, NonImplicitAssertions, WordSpec}

class Chapter03Spec extends WordSpec with Matchers with NonImplicitAssertions {

  "2.3 Exercise: The Truth About Monoids" should {
    import `2.3`.Monoid

    def associativeLaw[A](x: A, y: A, z: A)
      (implicit m: Monoid[A]): Boolean = {
      m.combine(x, m.combine(y, z)) ==
        m.combine(m.combine(x, y), z)
    }

    def identityLaw[A](x: A)
      (implicit m: Monoid[A]): Boolean = {
      (m.combine(x, m.empty) == x) &&
        (m.combine(m.empty, x) == x)
    }

    var bools = List(true, false)

    var allTripleBools = for {
      x <- bools
      y <- bools
      z <- bools
    } yield (x, y, z)


    "and monoid must satisfy monoid laws" in {
      import `2.3`.boolAndMonoid

      allTripleBools.forall(x => associativeLaw(x._1, x._2, x._3)) &&
        bools.forall(identityLaw(_)) shouldBe true
    }

    "or monoid must satisfy monoid laws" in {
      import `2.3`.boolOrMonoid

      allTripleBools.forall(x => associativeLaw(x._1, x._2, x._3)) &&
        bools.forall(identityLaw(_)) shouldBe true
    }

    "xor monoid must satisfy monoid laws" in {
      import `2.3`.boolXorMonoid

      allTripleBools.forall(x => associativeLaw(x._1, x._2, x._3)) &&
        bools.forall(identityLaw(_)) shouldBe true
    }

    "xnor monoid must satisfy monoid laws" in {
      import `2.3`.boolXnorMonoid

      allTripleBools.forall(x => associativeLaw(x._1, x._2, x._3)) &&
        bools.forall(identityLaw(_)) shouldBe true
    }

  }

  "2.4 Exercise: All Set for Monoids" should {
    import `2.3`.Monoid

    def associativeLaw[A](x: A, y: A, z: A)
      (implicit m: Monoid[A]): Boolean = {
      m.combine(x, m.combine(y, z)) ==
        m.combine(m.combine(x, y), z)
    }

    def identityLaw[A](x: A)
      (implicit m: Monoid[A]): Boolean = {
      (m.combine(x, m.empty) == x) &&
        (m.combine(m.empty, x) == x)
    }
    var tripleSet = (Set(1, 2), Set(2, 3), Set(3, 4))

    "set union monoid must satisfy monoid laws" in {
      import `2.4`._

      associativeLaw(tripleSet._1, tripleSet._2, tripleSet._3) &&
        identityLaw(tripleSet._1) shouldBe true
    }

    "set intersaction semigroup must satisfy associative laws" in {
      import `2.4`._

      associativeLaw(tripleSet._1, tripleSet._2, tripleSet._3) shouldBe true
    }
  }

  "2.5.4 Exercise: Adding All The Things" should {
    import `2.5.4`._
    import cats.instances.int._

    "return 5050 for following" in {
      add((1 to 100).toList) shouldBe 5050
    }

    "return Some(5050) for following" in {
      import cats.implicits._
      add((1 to 100).toList.map(_.some)) shouldBe 5050.some
    }

    "return Some(4950) for following" in {
      import cats.implicits._
      add(none :: ((1 to 99).toList.map(_.some))) shouldBe 4950.some
    }

    "return Order(800, 3) for following" in {
      add(List(Order(250, 1), Order(250, 1), Order(300, 1))) shouldBe Order(800, 3)
    }
  }

}
