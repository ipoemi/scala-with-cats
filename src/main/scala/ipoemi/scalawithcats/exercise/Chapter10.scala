package ipoemi.scalawithcats.exercise

object `10.3-3` {

  import cats.Semigroup
  import cats.instances.either._
  import cats.syntax.semigroup._

  /*
  trait Check[E, A] {
    self =>
    def apply(value: A): Either[E, A]

    def and(that: Check[E, A])(implicit se: Semigroup[E]): Check[E, A] =
      new Check[E, A] {
        def apply(value: A): Either[E, A] = (self(value), that(value)) match {
          case (Left(a), Left(b)) => Left(a |+| b)
          case (v @ Left(_), _) => v
          case (_, v @ Left(_)) => v
          case _ => Right(value)
        }
      }
  }

  object Check {
    def apply[E, A](fn: A => Either[E, A]): Check[E, A] =
      new Check[E, A] {
        def apply(value: A) = fn(value)
      }
  }
  */

  sealed trait Check[E, A] {
    def and(that: Check[E, A]): Check[E, A] = And(this, that)

    def apply(value: A)(implicit se: Semigroup[E]): Either[E, A] = this match {
      case Pure(func) => func(value)
      case And(left, right) => (left(value), right(value)) match {
        case (Left(a), Left(b)) => Left(a |+| b)
        case (v @ Left(_), _) => v
        case (_, v @ Left(_)) => v
        case _ => Right(value)
      }
    }
  }

  final case class And[E, A](left: Check[E, A], right: Check[E, A]) extends Check[E, A]

  final case class Pure[E, A](func: A => Either[E, A]) extends Check[E, A]

  object Check {
    def apply[E, A](func: A => Either[E, A]) = Pure(func)
  }

}

object `10.3-4` {

  import cats.Semigroup
  import cats.data.Validated
  import cats.data.Validated._
  import cats.syntax.semigroup._
  import cats.syntax.apply._

  sealed trait Check[E, A] {
    def and(that: Check[E, A]): Check[E, A] = And(this, that)

    def apply(value: A)(implicit se: Semigroup[E]): Validated[E, A] = this match {
      case Pure(func) => func(value)
      case And(left, right) => (left(value), right(value)).mapN((_, _) => value)
    }
  }

  final case class And[E, A](left: Check[E, A], right: Check[E, A]) extends Check[E, A]

  final case class Pure[E, A](func: A => Validated[E, A]) extends Check[E, A]

  object Check {
    def apply[E, A](func: A => Validated[E, A]) = Pure(func)
  }

}

object `10.3-5` {

  import cats.Semigroup
  import cats.data.Validated
  import cats.data.Validated._
  import cats.syntax.semigroup._
  import cats.syntax.apply._

  sealed trait Check[E, A] {
    def and(that: Check[E, A]): Check[E, A] = And(this, that)

    def or(that: Check[E, A]): Check[E, A] = Or(this, that)

    def apply(value: A)(implicit se: Semigroup[E]): Validated[E, A] = this match {
      case Pure(func) => func(value)
      case And(left, right) => (left(value), right(value)).mapN((_, _) => value)
      case Or(left, right) => (left(value), right(value)) match {
        case (a @ Valid(_), _) => a
        case (_, a @ Valid(_)) => a
        case (Invalid(a), Invalid(b)) => invalid(a |+| b)
      }
    }
  }

  final case class And[E, A](left: Check[E, A], right: Check[E, A]) extends Check[E, A]

  final case class Or[E, A](left: Check[E, A], right: Check[E, A]) extends Check[E, A]

  final case class Pure[E, A](func: A => Validated[E, A]) extends Check[E, A]

  object Check {
    def apply[E, A](func: A => Validated[E, A]) = Pure(func)
  }

}

object `10.4.2` {

  import cats.Semigroup
  import cats.data.Validated
  import cats.data.Validated._
  import cats.syntax.semigroup._
  import cats.syntax.apply._

  sealed trait Predicate[E, A] {

    import Predicate._

    def and(that: Predicate[E, A]): Predicate[E, A] = And(this, that)

    def or(that: Predicate[E, A]): Predicate[E, A] = Or(this, that)

    def apply(value: A)(implicit se: Semigroup[E]): Validated[E, A] = this match {
      case Pure(func) => func(value)
      case And(left, right) => (left(value), right(value)).mapN((_, _) => value)
      case Or(left, right) => (left(value), right(value)) match {
        case (a @ Valid(_), _) => a
        case (_, a @ Valid(_)) => a
        case (Invalid(a), Invalid(b)) => invalid(a |+| b)
      }
    }
  }

  object Predicate {

    final case class And[E, A](left: Predicate[E, A], right: Predicate[E, A]) extends Predicate[E, A]

    final case class Or[E, A](left: Predicate[E, A], right: Predicate[E, A]) extends Predicate[E, A]

    final case class Pure[E, A](func: A => Validated[E, A]) extends Predicate[E, A]

    def apply[E, A](func: A => Validated[E, A]) = Pure(func)

    def lift[E, A](onError: => E, fn: A => Boolean): Predicate[E, A] = {
      lazy val error = onError
      Pure(a => if (fn(a)) valid(a) else invalid(error))
    }
  }


  sealed trait Check[E, A, B] {

    import Check._

    def apply(a: A)(implicit se: Semigroup[E]): Validated[E, B]

    def map[C](func: B => C): Check[E, A, C] = Map(this, func)

    def flatMap[C](func: B => Check[E, A, C]): Check[E, A, C] = FlatMap(this, func)

    def andThen[C](that: Check[E, B, C]): Check[E, A, C] = AndThen(this, that)
  }

  object Check {

    final case class AndThen[E, A, B, C](left: Check[E, A, B], right: Check[E, B, C]) extends Check[E, A, C] {
      /*
      def apply(a: A)(implicit se: Semigroup[E]): Validated[E, C] = left(a) match {
        case Valid(b) => right(b)
        case Invalid(e) => invalid[E, C](e)
      }
      */
      def apply(a: A)(implicit se: Semigroup[E]): Validated[E, C] =
        left(a).withEither(_.flatMap(right(_).toEither))
    }

    final case class FlatMap[E, A, B, C](c: Check[E, A, B], fn: B => Check[E, A, C]) extends Check[E, A, C] {
      /*
      def apply(a: A)(implicit se: Semigroup[E]): Validated[E, C] = c(a) match {
        case Valid(b) => fn(b)(a)
        case Invalid(e) => invalid[E, C](e)
      }
      */
      def apply(a: A)(implicit se: Semigroup[E]): Validated[E, C] =
        c(a).withEither(_.flatMap(fn(_)(a).toEither))
    }

    final case class Map[E, A, B, C](c: Check[E, A, B], fn: B => C) extends Check[E, A, C] {
      def apply(a: A)(implicit se: Semigroup[E]): Validated[E, C] = c(a).map(fn)
    }

    final case class Pure[E, A, B](fn: A => Validated[E, B]) extends Check[E, A, B] {
      def apply(a: A)(implicit se: Semigroup[E]): Validated[E, B] = fn(a)
    }

    final case class PurePredicate[E, A](pred: Predicate[E, A]) extends Check[E, A, A] {
      def apply(a: A)(implicit se: Semigroup[E]): Validated[E, A] = pred(a)
    }

    def apply[E, A, B](fn: A => Validated[E, B]): Check[E, A, B] = Pure(fn)

    def apply[E, A](pred: Predicate[E, A]): Check[E, A, A] = PurePredicate(pred)

  }


}

object `10.4.3` {

  import `10.4.2`._

  import cats.syntax.apply._
  import cats.data.{NonEmptyList, Validated}

  type Errors = NonEmptyList[String]

  def error(s: String): NonEmptyList[String] =
    NonEmptyList(s, Nil)

  /*
  def longerThan(n: Int): Predicate[Errors, String] =
    Predicate.lift(
      error(s"Must be longer than $n characters"),
      str => str.size > n)

  val alphanumeric: Predicate[Errors, String] =
    Predicate.lift(
      error(s"Must be all alphanumeric characters"),
      str => str.forall(_.isLetterOrDigit))

  def contains(char: Char): Predicate[Errors, String] =
    Predicate.lift(
      error(s"Must contain the character $char"),
      str => str.contains(char))

  def containsOnce(char: Char): Predicate[Errors, String] =
    Predicate.lift(
      error(s"Must contain the character $char only once"),
      str => str.filter(c => c == char).size == 1)
  */
  def longerThan(n: Int)(name: String): Predicate[Errors, String] =
    Predicate.lift(
      error(s"$name must be longer than $n characters"),
      str => str.size > n)

  def alphanumeric(name: String): Predicate[Errors, String] =
    Predicate.lift(
      error(s"$name must be all alphanumeric characters"),
      str => str.forall(_.isLetterOrDigit))

  def contains(char: Char)(name: String): Predicate[Errors, String] =
    Predicate.lift(
      error(s"$name must contain the character $char"),
      str => str.contains(char))

  def containsOnce(char: Char)(name: String): Predicate[Errors, String] =
    Predicate.lift(
      error(s"$name must contain the character $char only once"),
      str => str.filter(c => c == char).size == 1)

  val checkUserName: Check[Errors, String, String] =
    Check(longerThan(3)("UserName") and alphanumeric("UserName"))

  val splitEmail: Check[Errors, String, (String, String)] =
    Check(containsOnce('@')("Email")).map { s =>
      val Array(name, domain) = s.split('@')
      (name, domain)
    }

  val checkEmailName: Check[Errors, String, String] =
    Check(longerThan(0)("Email-Name"))

  val checkEmailDomain: Check[Errors, String, String] =
    Check(longerThan(3)("Email-Domain") and contains('.')("Email-Domain"))

  val joinEmail: Check[Errors, (String, String), String] =
    Check {
      case (l, r) =>
        (checkEmailName(l), checkEmailDomain(r)).mapN(_ + '@' + _)
    }

  val checkEmail: Check[Errors, String, String] = splitEmail andThen joinEmail

  def checkUser(userName: String, email: String): Validated[Errors, (String, String)] =
    (checkUserName(userName), checkEmail(email)).tupled
}

object `10.5` {

  import cats.Semigroup
  import cats.data.{NonEmptyList, Kleisli, Validated}
  import cats.data.Validated._
  import cats.instances.either._
  import cats.syntax.either._
  import cats.syntax.semigroup._
  import cats.syntax.apply._

  sealed trait Predicate[E, A] {

    import Predicate._

    def and(that: Predicate[E, A]): Predicate[E, A] = And(this, that)

    def or(that: Predicate[E, A]): Predicate[E, A] = Or(this, that)

    def apply(value: A)(implicit se: Semigroup[E]): Validated[E, A] = this match {
      case Pure(func) => func(value)
      case And(left, right) => (left(value), right(value)).mapN((_, _) => value)
      case Or(left, right) => (left(value), right(value)) match {
        case (a @ Valid(_), _) => a
        case (_, a @ Valid(_)) => a
        case (Invalid(a), Invalid(b)) => invalid(a |+| b)
      }
    }

    def run(implicit se: Semigroup[E]): A => Either[E, A] = (a: A) => apply(a).toEither
  }

  object Predicate {

    final case class And[E, A](left: Predicate[E, A], right: Predicate[E, A]) extends Predicate[E, A]

    final case class Or[E, A](left: Predicate[E, A], right: Predicate[E, A]) extends Predicate[E, A]

    final case class Pure[E, A](func: A => Validated[E, A]) extends Predicate[E, A]

    def apply[E, A](func: A => Validated[E, A]) = Pure(func)

    def lift[E, A](onError: => E, fn: A => Boolean): Predicate[E, A] = {
      lazy val error = onError
      Pure(a => if (fn(a)) valid(a) else invalid(error))
    }
  }

  type Errors = NonEmptyList[String]

  type ErrorsOr[A] = Either[Errors, A]

  def error(s: String): NonEmptyList[String] =
    NonEmptyList(s, Nil)

  def longerThan(n: Int)(name: String): Predicate[Errors, String] =
    Predicate.lift(
      error(s"$name must be longer than $n characters"),
      str => str.size > n)

  def alphanumeric(name: String): Predicate[Errors, String] =
    Predicate.lift(
      error(s"$name must be all alphanumeric characters"),
      str => str.forall(_.isLetterOrDigit))

  def contains(char: Char)(name: String): Predicate[Errors, String] =
    Predicate.lift(
      error(s"$name must contain the character $char"),
      str => str.contains(char))

  def containsOnce(char: Char)(name: String): Predicate[Errors, String] =
    Predicate.lift(
      error(s"$name must contain the character $char only once"),
      str => str.filter(c => c == char).size == 1)

  val checkUserName: Kleisli[ErrorsOr, String, String] =
    Kleisli((longerThan(3)("UserName") and alphanumeric("UserName")).run)

  val splitEmail: Kleisli[ErrorsOr, String, (String, String)] =
    Kleisli(containsOnce('@')("Email").run).map { s =>
      val Array(name, domain) = s.split('@')
      (name, domain)
    }

  val checkEmailName: Kleisli[ErrorsOr, String, String] =
    Kleisli(longerThan(0)("Email-Name").run)

  val checkEmailDomain: Kleisli[ErrorsOr, String, String] =
    Kleisli((longerThan(3)("Email-Domain") and contains('.')("Email-Domain")).run)

  val joinEmail: Kleisli[ErrorsOr, (String, String), String] =
    Kleisli {
      case (l, r) =>
        (checkEmailName(l), checkEmailDomain(r)).mapN(_ + '@' + _)
    }

  val checkEmail: Kleisli[ErrorsOr, String, String] = splitEmail andThen joinEmail

  def checkUser(userName: String, email: String): Validated[Errors, (String, String)] =
    (checkUserName(userName).toValidated, checkEmail(email).toValidated).tupled

}
