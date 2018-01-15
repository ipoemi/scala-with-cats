package ipoemi.scalawithcats.exercise

object `11.2.3` {

  final case class GCounter(counters: Map[String, Int]) {
    def increment(machine: String, amount: Int) =
      GCounter(counters.updated(machine, counters.getOrElse(machine, 0) + amount))

    def merge(that: GCounter): GCounter = {
      val keyValues = (counters.keySet ++ that.counters.keySet).map({ key =>
        val value = counters.getOrElse(key, 0) max (that.counters.getOrElse(key, 0))
        key -> value
      }).toList
      GCounter(Map(keyValues: _*))
    }

    def total: Int = counters.values.sum

  }
}

object `11.3.2` {
  import cats.Monoid

  trait BoundedSemiLattice[A] extends Monoid[A] {
    def combine(a1: A, a2: A): A
    def empty: A
  }

  object BoundedSemiLattice {
    def apply[A](implicit B: BoundedSemiLattice[A]) = B

    implicit val intBoudedSemiLattice = new BoundedSemiLattice[Int] {
      def combine(a1: Int, a2: Int): Int = a1 max a2
      def empty: Int = 0
    }

    implicit def setBoudedSemiLattice[A] = new BoundedSemiLattice[Set[A]] {
      def combine(a1: Set[A], a2: Set[A]): Set[A] = a1 ++ a2
      def empty: Set[A] = Set.empty[A]
    }
  }

}

object `11.3.3` {
  import cats.Monoid
  import cats.instances.map._
  import cats.syntax.monoid._

  import `11.3.2`._

  final case class GCounter[A](counters: Map[String, A]) {
    def increment(machine: String, amount: A)(implicit M: Monoid[A]) =
      GCounter(counters.updated(machine, counters.getOrElse(machine, M.empty) |+| amount))

    def merge(that: GCounter[A])(implicit B: BoundedSemiLattice[A]): GCounter[A] = {
      GCounter(that.counters |+| counters)
    }

    def total(implicit M: Monoid[A]): A = M.combineAll(counters.values)

  }
}

