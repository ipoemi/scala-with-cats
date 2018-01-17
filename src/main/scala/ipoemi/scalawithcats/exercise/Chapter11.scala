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
    import cats.syntax.semigroup._
    import cats.instances.map._

    def apply[A](implicit B: BoundedSemiLattice[A]) = B

    implicit val intBoudedSemiLattice = new BoundedSemiLattice[Int] {
      def combine(a1: Int, a2: Int): Int = a1 max a2

      def empty: Int = 0
    }

    implicit def setBoudedSemiLattice[A] = new BoundedSemiLattice[Set[A]] {
      def combine(a1: Set[A], a2: Set[A]): Set[A] = a1 union a2

      def empty: Set[A] = Set.empty[A]
    }

    implicit def mapBoudedSemiLattice[K, V](implicit b: BoundedSemiLattice[V]) =
      new BoundedSemiLattice[Map[K, V]] {
        def combine(a1: Map[K, V], a2: Map[K, V]): Map[K, V] =
          a1 |+| a2

        def empty: Map[K, V] = Map.empty[K, V]
      }
  }

}

object `11.3.3` {

  import `11.3.2`._
  import cats.Monoid
  import cats.instances.map._
  import cats.syntax.semigroup._

  final case class GCounter[A](counters: Map[String, A]) {
    def increment(machine: String, amount: A)(implicit M: Monoid[A]) =
      GCounter(counters.updated(machine, counters.getOrElse(machine, M.empty) |+| amount))

    def merge(that: GCounter[A])(implicit B: BoundedSemiLattice[A]): GCounter[A] = {
      GCounter(that.counters |+| counters)
    }

    def total(implicit M: Monoid[A]): A = M.combineAll(counters.values)

  }

}

object `11.4` {

  import `11.3.2`.BoundedSemiLattice
  import cats.Monoid
  import cats.instances.map._
  import cats.syntax.semigroup._

  trait GCounter[F[_, _], K, V] {
    def increment(f: F[K, V])(k: K, v: V)(implicit m: Monoid[V]): F[K, V]

    def merge(f1: F[K, V], f2: F[K, V])(implicit b: BoundedSemiLattice[V]): F[K, V]

    def total(f: F[K, V])(implicit m: Monoid[V]): V
  }

  object GCounter {
    def apply[F[_, _], K, V](implicit counter: GCounter[F, K, V]) = counter

    implicit def mapGCount[K, V] = new GCounter[Map, K, V] {
      def increment(f: Map[K, V])(k: K, v: V)(implicit m: Monoid[V]): Map[K, V] =
        f + (k -> (v |+| f.getOrElse(k, m.empty)))

      def merge(f1: Map[K, V], f2: Map[K, V])(implicit b: BoundedSemiLattice[V]): Map[K, V] =
        f1 |+| f2

      def total(f: Map[K, V])(implicit m: Monoid[V]): V =
        m.combineAll(f.values)
    }

  }

}

object `11.5` {

  import cats.Monoid
  import cats.instances.list._
  import cats.syntax.foldable._
  import cats.syntax.semigroup._

  // Monoid Dependant Definition
  trait BoundedSemiLattice[A] {
    def combine(a1: A, a2: A): A

    def empty: A
  }

  object BoundedSemiLattice {
    def apply[A](implicit B: BoundedSemiLattice[A]) = B

  }

  object BoundedSemiLatticeInstances {
    implicit val intBoudedSemiLattice = new BoundedSemiLattice[Int] {
      def combine(a1: Int, a2: Int): Int = a1 max a2

      def empty: Int = 0
    }

    implicit def setBoudedSemiLattice[A] = new BoundedSemiLattice[Set[A]] {
      def combine(a1: Set[A], a2: Set[A]): Set[A] = a1 union a2

      def empty: Set[A] = Set.empty[A]
    }

  }

  trait GCounter[F[_, _], K, V] {
    def increment(f: F[K, V])(k: K, v: V)(implicit m: Monoid[V]): F[K, V]

    def merge(f1: F[K, V], f2: F[K, V])(implicit b: BoundedSemiLattice[V]): F[K, V]

    def total(f: F[K, V])(implicit m: Monoid[V], fm: Monoid[F[K, V]]): V
  }

  object GCounter {
    def apply[F[_, _], K, V](implicit counter: GCounter[F, K, V]) = counter
  }

  object GCounterInstances {
    import KeyValueStore._

    implicit def gCounterInstance[F[_, _], K, V](implicit kvs: KeyValueStore[F]) =
      new GCounter[F, K, V] {
        def increment(f: F[K, V])(key: K, value: V)(implicit m: Monoid[V]): F[K, V] = {
          val total = f.getOrElse(key, m.empty) |+| value
          f.put(key, total)
        }

        def merge(f1: F[K, V], f2: F[K, V])(implicit b: BoundedSemiLattice[V]): F[K, V] =
          f2.keys.map { key =>
            (key, b.combine(f1.get(key).getOrElse(b.empty), f2.get(key).get))
          }.foldLeft(f1) { (f, kv) =>
            f.put(kv._1, kv._2)
          }

        def total(f: F[K, V])(implicit m: Monoid[V], km: Monoid[F[K, V]]): V =
          f.values.combineAll
      }

  }

  trait KeyValueStore[F[_, _]] {
    def put[K, V](f: F[K, V])(k: K, v: V): F[K, V]

    def get[K, V](f: F[K, V])(k: K): Option[V]

    def getOrElse[K, V](f: F[K, V])(k: K, default: V): V = get(f)(k).getOrElse(default)

    def values[K, V](f: F[K, V]): List[V]

    def keys[K, V](f: F[K, V]): List[K]
  }

  object KeyValueStore {

    def apply[F[_, _]](implicit kvs: KeyValueStore[F]) = kvs

    implicit class KvsOps[F[_, _], K, V](f: F[K, V]) {
      def put(key: K, value: V)(implicit kvs: KeyValueStore[F]): F[K, V] =
        kvs.put(f)(key, value)

      def get(key: K)(implicit kvs: KeyValueStore[F]): Option[V] =
        kvs.get(f)(key)

      def getOrElse(key: K, default: V)(implicit kvs: KeyValueStore[F]): V =
        kvs.getOrElse(f)(key, default)

      def values(implicit kvs: KeyValueStore[F]): List[V] =
        kvs.values(f)

      def keys(implicit kvs: KeyValueStore[F]): List[K] =
        kvs.keys(f)
    }

  }

  object KeyValueStoreInstances {
    implicit val mapKeyValueStore = new KeyValueStore[Map] {
      def put[K, V](f: Map[K, V])(k: K, v: V): Map[K, V] = f + (k -> v)

      def get[K, V](f: Map[K, V])(k: K): Option[V] = f.get(k)

      def values[K, V](f: Map[K, V]): List[V] = f.values.toList

      def keys[K, V](f: Map[K, V]): List[K] = f.keys.toList
    }

  }

}

