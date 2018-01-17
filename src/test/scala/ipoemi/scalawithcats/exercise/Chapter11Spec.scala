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

    "intBoundedSemiLattice must satisfy the following" in {
      import `11.3.2`.BoundedSemiLattice._
      import cats.Monoid
      import cats.instances.either._
      import cats.instances.map._
      import cats.instances.option._
      import cats.syntax.either._
      import cats.syntax.option._
      import cats.syntax.semigroup._
      //import cats.instances.int._

      (1 |+| 2) shouldBe 2
      (2 |+| 1) shouldBe 2
      (2 |+| Monoid[Int].empty) shouldBe 2
      (Monoid[Int].empty |+| 2) shouldBe 2
      (1.some |+| 2.some) shouldBe 2.some
      (1.asRight[String] |+| 2.asRight[String]) shouldBe 2.asRight[String]
      (Map("key1" -> 1, "key2" -> 2) |+| Map("key1" -> 2)) shouldBe Map("key1" -> 2, "key2" -> 2)
    }

    "setBoundedSemiLattice must satisfy the following" in {
      import `11.3.2`.BoundedSemiLattice._
      import cats.Monoid
      import cats.instances.either._
      import cats.instances.map._
      import cats.instances.option._
      import cats.syntax.either._
      import cats.syntax.option._
      import cats.syntax.semigroup._

      (Set(1) |+| Set(2)) shouldBe Set(1, 2)
      (Set(2) |+| Set(1)) shouldBe Set(2, 1)
      (Set(3, 4, 5) |+| Monoid[Set[Int]].empty) shouldBe Set(3, 4, 5)
      (Monoid[Set[Int]].empty |+| Set(3, 4, 5)) shouldBe Set(3, 4, 5)
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

  "11.4 Abstracting GCounter to a Type Class" should {
    import `11.4`._
    //import `11.3.2`.BoundedSemiLattice._
    import cats.instances.int._

    "GCounter[Map, String, Int] must satisfy the following" in {
      val g = GCounter[Map, String, Int]
      g.increment(Map("A" -> 1, "B" -> 2))("A", 2) shouldBe Map("A" -> 3, "B" -> 2)

      g.merge(Map("A" -> 1, "B" -> 2), Map("A" -> 2, "B" -> 2)) shouldBe
        Map("A" -> 2, "B" -> 2) // int BoundedSemiLattice instance

      g.total(Map("A" -> 1, "B" -> 2)) shouldBe 3
    }

    "GCounter[Map, String, Int] must satisfy the following2`" in {
      val g1 = Map("a" -> 7, "b" -> 3)
      val g2 = Map("a" -> 2, "b" -> 5)

      val counter = GCounter[Map, String, Int]

      counter.merge(g1, g2) shouldBe Map("a" -> 7, "b" -> 5)

      counter.total(counter.merge(g1, g2)) shouldBe 12
    }

  }

  "11.5 Abstracting a Key Value Store" should {
    import `11.5`._
    import `11.5`.BoundedSemiLatticeInstances._
    import `11.5`.KeyValueStoreInstances._
    import `11.5`.GCounterInstances._
    import cats.Monoid
    import cats.instances.map._
    import cats.instances.int._

    val counter = GCounter[Map, String, Int]

    val g1 = Map("a" -> 7, "b" -> 3)
    val g2 = Map("a" -> 2, "b" -> 5)

    "GCounter[Map, String, Int].increment must satisfy the following" in {
      counter.increment(Map("A" -> 1, "B" -> 2))("A", 2) shouldBe Map("A" -> 3, "B" -> 2)
    }

    "GCounter[Map, String, Int].merge must satisfy the following" in {
      counter.merge(Map("A" -> 1, "B" -> 2), Map("A" -> 2, "B" -> 2)) shouldBe Map("A" -> 2, "B" -> 2)
      counter.merge(g1, g2) shouldBe Map("a" -> 7, "b" -> 5)
    }

    "GCounter[Map, String, Int].total must satisfy the following`" in {
      counter.total(counter.merge(g1, g2)) shouldBe 12
      counter.total(Map("A" -> 1, "B" -> 2)) shouldBe 3
    }

  }

}
