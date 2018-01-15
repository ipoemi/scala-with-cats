package ipoemi.scalawithcats.exercise

import org.scalatest.{Matchers, WordSpec}

class Chapter11Spec extends WordSpec with Matchers {

  "11.2.3 Exercise: GCounter Implementation" should {
    import `11.2.3`._

    "return GCounter(Map(\"A\" -> 1, \"B\" -> 2)) " in {
      GCounter(Map("A" -> 1, "B" -> 1)).increment("B", 1) shouldBe GCounter(Map("A" -> 1, "B" -> 2))
    }

    "return GCounter(Map(\"A\" -> 5, \"B\" -> 3, \"C\" -> 5) " in {

      var gCounter1 = GCounter(Map("A" -> 2, "B" -> 3))
      var gCounter2 = GCounter(Map("A" -> 5, "B" -> 3))
      var gCounter3 = GCounter(Map("A" -> 1, "B" -> 2, "C" -> 5))

      gCounter1.merge(gCounter2).merge(gCounter3) shouldBe GCounter(Map("A" -> 5, "B" -> 3, "C" -> 5))
    }

    "return 13 " in {

      var gCounter1 = GCounter(Map("A" -> 2, "B" -> 3))
      var gCounter2 = GCounter(Map("A" -> 5, "B" -> 3))
      var gCounter3 = GCounter(Map("A" -> 1, "B" -> 2, "C" -> 5))

      gCounter1.merge(gCounter2).merge(gCounter3).total shouldBe 13
    }
  }

  "11.3.2 Exercise: BoundedSemiLattice Instances" should {
    import `11.3.2`._

    import BoundedSemiLattice._

    "intBoundedSemiLattice must satisfy the following" in {
      import cats.instances.list._
      import cats.instances.either._
      import cats.instances.option._
      import cats.instances.map._
      import cats.syntax.either._
      import cats.syntax.option._
      import cats.syntax.semigroup._

      (1 |+| 2) shouldBe 2
      (2 |+| 1) shouldBe 2
      (2 |+| BoundedSemiLattice[Int].empty) shouldBe 2
      (BoundedSemiLattice[Int].empty |+| 2) shouldBe 2
      (1.some |+| 2.some) shouldBe 2.some
      (1.asRight[String] |+| 2.asRight[String]) shouldBe 2.asRight[String]
      (Map("key1" -> 1, "key2" -> 2) |+| Map("key1" -> 2)) shouldBe Map("key1" -> 2, "key2" -> 2)
    }

    "setBoundedSemiLattice must satisfy the following" in {
      import cats.instances.list._
      import cats.instances.either._
      import cats.instances.option._
      import cats.instances.map._
      import cats.syntax.either._
      import cats.syntax.option._
      import cats.syntax.semigroup._

      (Set(1) |+| Set(2)) shouldBe Set(1, 2)
      (Set(2) |+| Set(1)) shouldBe Set(2, 1)
      (Set(3, 4, 5) |+| BoundedSemiLattice[Set[Int]].empty) shouldBe Set(3, 4, 5)
      (BoundedSemiLattice[Set[Int]].empty |+| Set(3, 4, 5)) shouldBe Set(3, 4, 5)
      (Set(3, 4, 5).some |+| Set(4, 6, 7).some) shouldBe Set(3, 4, 5, 6, 7).some
      (Set(3, 4, 5).asRight[String] |+| Set(4, 6, 7).asRight[String]) shouldBe Set(3, 4, 5, 6, 7).asRight[String]
      (Map("key1" -> Set(3, 4, 5), "key2" -> Set(4, 5, 6)) |+| Map("key1" -> Set(5, 6, 7))) shouldBe
        Map("key1" -> Set(3, 4, 5, 6, 7), "key2" -> Set(4, 5, 6))
    }
  }

  "11.3.3 Exercise: Generic GCounter" should {
    import `11.3.3`._

    import cats.instances.int._

    "GCounter must satisfy the following" in {
      GCounter(Map("A" -> 1, "B" -> 2)).increment("A", 2) shouldBe
        GCounter(Map("A" -> 3, "B" -> 2)) // int Monoid instance

      GCounter(Map("A" -> 1, "B" -> 2)).merge(GCounter(Map("A" -> 2, "B" -> 2))) shouldBe
        GCounter(Map("A" -> 2, "B" -> 2)) // int BoundedSemiLattice instance

      GCounter(Map("A" -> 1, "B" -> 2)).total shouldBe 3
    }

  }

}
