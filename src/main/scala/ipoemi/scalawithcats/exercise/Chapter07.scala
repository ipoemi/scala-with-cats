package ipoemi.scalawithcats.exercise

object `7.1.2` {
  import cats.Monad

  List().foldLeft(List.empty)((xs, x) => x :: xs)
  List().foldRight(List.empty)((x, xs) => x :: xs)
}

object `7.1.3` {

  import cats.Monoid

  def map[A, B](xs: List[A])(fn: A => B): List[B] =
    xs.foldRight(List.empty[B])((x, xs) => fn(x) :: xs)

  def flatMap[A, B](xs: List[A])(fn: A => List[B]): List[B] =
    xs.foldRight(List.empty[B])((x, xs) => fn(x) ++ xs)

  def filter[A](xs: List[A])(fn: A => Boolean): List[A] =
    xs.foldRight(List.empty[A])((x, xs) => if (fn(x)) x :: xs else xs)

  /*
  def sum[A: Monoid](xs: List[A]): A =
    xs.foldRight(Monoid[A].empty)((x, xs) => Monoid[A].combine(x, xs))
  */
  def sum[A](xs: List[A])(implicit num: Numeric[A]): A =
    xs.foldRight(num.zero)(num.plus)
}

object `7.2.2.1` {
  import cats.Applicative
  import cats.syntax.applicative._
  import cats.syntax.apply._
  import cats.instances.vector._ // for Applicative

  def listTraverse[F[_] : Applicative, A, B](list: List[A])(func: A => F[B]): F[List[B]] =
    list.foldLeft(List.empty[B].pure[F]) { (accum, item) => (accum, func(item)).mapN(_ :+ _)
    }

  def listSequence[F[_] : Applicative, B](list: List[F[B]]): F[List[B]] =
    listTraverse(list)(identity)

}

object `7.2.2.2` {
  import `7.2.2.1`._
  import cats.instances.option._

  def process(inputs: List[Int]): Option[List[Int]] =
    listTraverse(inputs)(n => if (n % 2 == 0) Some(n) else None)
}

object `7.2.2.3` {
  import `7.2.2.1`._

  import cats.data.Validated
  import cats.instances.list._ // for Monoid

  type ErrorsOr[A] = Validated[List[String], A]

  def process(inputs: List[Int]): ErrorsOr[List[Int]] =
    listTraverse(inputs) { n =>
      if (n % 2 == 0) {
        Validated.valid(n)
      } else {
        Validated.invalid(List(s"$n is not even"))
      }
    }
}
