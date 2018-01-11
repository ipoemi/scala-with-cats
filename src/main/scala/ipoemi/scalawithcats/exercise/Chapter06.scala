package ipoemi.scalawithcats.exercise

object `6.3.1.1` {
  import cats.Monad

  def product[M[_] : Monad, A, B](x: M[A], y: M[B]): M[(A, B)] =
    Monad[M].flatMap(x)(a => Monad[M].map(y)(b => (a, b)))
}

object `6.4.4` {
  case class User(name: String, age: Int)

  import cats.data.Validated
  import cats.data.Validated._
  import cats.syntax.either._
  import cats.syntax.apply._
  import cats.instances.list._
  import cats.instances.string._

  type ErrorOr[A] = Either[List[String], A]

  type AllErrorsOr[A] = Validated[List[String], A]

  def getValue[K, V](k: K)(m: Map[K, V]): Option[V] = m.get(k)

  def parseInt(s: String): Option[Int] = Either.catchNonFatal(s.toInt).toOption

  def nonBlank(s: String): Boolean = s.nonEmpty

  def nonNegative(n: Int): Boolean = n >= 0

  def readName(m: Map[String, String]): ErrorOr[String] =
    getValue("name")(m)
      .toRight(List("the name and age must be specified"))
      .ensure(List("the name must not be blank"))(nonBlank)

  def readAge(m: Map[String, String]): ErrorOr[Int] =
    for {
      ageStr <- getValue("age")(m).toRight(List("the name and age must be specified"))
      age <- parseInt(ageStr).toRight(List("the age must be a valid non-negative integer"))
    } yield age

  def readUser(m: Map[String, String]): AllErrorsOr[User] =
    (readName(m).toValidated, readAge(m).toValidated).mapN(User.apply)

}

