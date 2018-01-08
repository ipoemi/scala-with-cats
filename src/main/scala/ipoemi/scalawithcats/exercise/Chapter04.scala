package ipoemi.scalawithcats.exercise

import scala.language.higherKinds
import scala.language.postfixOps

object `4.1.2` {

  trait Monad[F[_]] {
    def pure[A](a: A): F[A]

    def flatMap[A, B](value: F[A])(func: A => F[B]): F[B]

    def map[A, B](value: F[A])(func: A => B): F[B] =
      flatMap(value)(x => pure(func(x)))
  }

}

object `4.3.1` {

  import `4.1.2`._
  import cats.Id

  implicit val idMonad = new Monad[Id] {
    def pure[A](a: A): Id[A] = a
    def flatMap[A, B](value: Id[A])(func: A => Id[B]): Id[B] = func(value)
  }
}

object `4.6.5` {

  import cats.Eval

  def foldRight[A, B](as: List[A], acc: B)(fn: (A, B) => B): B = {
    def aux(as: List[A])(fn: (A, B) => B): Eval[B] =
      as match {
        case head :: tail =>
          Eval.defer(aux(tail)(fn).map(fn(head, _)))
        case Nil =>
          Eval.now(acc)
      }

    aux(as)(fn).value
  }

}

object `4.7.3` {
  def slowly[A](body: => A) =
    try body finally Thread.sleep(100)

  def factorial(n: Int): Int = {
    val ans = slowly(if (n == 0) 1 else n * factorial(n - 1))
    println(s"fact $n $ans")
    ans
  }

  import cats.implicits._
  import cats.data.Writer

  type Logged[A] = Writer[Vector[String], A]

  def factorial2(n: Int): Int = {
    def aux(n: Int): Logged[Int] =
      slowly {
        for {
          ans <-
            if (n == 0) 1.pure[Logged]
            else aux(n - 1).map(_ * n)
          _ <- Vector(s"fact $n $ans").tell
        } yield ans
      }

    val ans = aux(n)
    ans.written.foreach(println)
    ans.value
  }
}

object `4.8.3` {

  case class Db(
    usernames: Map[Int, String],
    passwords: Map[String, String]
  )

  import cats.data.Reader
  import cats.implicits._

  type DbReader[A] = Reader[Db, A]

  def findUsername(userId: Int): DbReader[Option[String]] =
    Reader((db: Db) => db.usernames.get(userId))

  def checkPassword(username: String, password: String): DbReader[Boolean] =
    Reader((db: Db) => db.passwords.get(username) === password.some)

  def checkLogin(userId: Int, password: String): DbReader[Boolean] =
    for {
      usernameOpt <- findUsername(userId)
      ans <- usernameOpt match {
        case Some(username) => checkPassword(username, password)
        case None => false.pure[DbReader]
      }
    } yield ans

}

object `4.9.3` {

}

