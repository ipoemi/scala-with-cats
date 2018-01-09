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
  import cats.data.State
  import cats.syntax.applicative._

  type CalcState[A] = State[List[Int], A]

  /*
  def evalOne(sym: String): CalcState[Int] = sym match {
    case "+" =>
      State[List[Int], Int] { oldStack =>
        val x = oldStack.head
        val y = oldStack.tail.head
        val result = x + y
        val newStack = result :: oldStack.tail.tail
        (newStack, result)
      }
    case "-" =>
      State[List[Int], Int] { oldStack =>
        val x = oldStack.head
        val y = oldStack.tail.head
        val result = x - y
        val newStack = result :: oldStack.tail.tail
        (newStack, result)
      }
    case "*" =>
      State[List[Int], Int] { oldStack =>
        val x = oldStack.head
        val y = oldStack.tail.head
        val result = x * y
        val newStack = result :: oldStack.tail.tail
        (newStack, result)
      }
    case "/" =>
      State[List[Int], Int] { oldStack =>
        val x = oldStack.head
        val y = oldStack.tail.head
        val result = x / y
        val newStack = result :: oldStack.tail.tail
        (newStack, result)
      }
    case numStr =>
      State[List[Int], Int] { oldStack =>
        var num = numStr.toInt
        val newStack = num :: oldStack
        val result = num
        (newStack, result)
      }
  }
  */
  def evalOne(sym: String): CalcState[Int] = sym match {
    case "+" => operator(_ + _)
    case "-" => operator(_ - _)
    case "*" => operator(_ * _)
    case "/" => operator(_ / _)
    case num => operand(num)
  }

  def operator(func: (Int, Int) => Int): CalcState[Int] =
    State[List[Int], Int] {
      case x :: y :: stack => (func(x, y) :: stack, func(x, y))
      case _ => sys.exit(1)
    }

  def operand(num: String): CalcState[Int] =
    State[List[Int], Int](x => (num.toInt :: x, num.toInt))

  def evalAll(input: List[String]): CalcState[Int] =
  //input.tail.foldLeft(evalOne(input.head))((state, sym) => state.flatMap(x => evalOne(sym)))
    input.foldLeft(0.pure[CalcState])((state, sym) => state.flatMap(x => evalOne(sym)))
}

object `4.10.1` {
  sealed trait Tree[+A]

  final case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

  final case class Leaf[A](value: A) extends Tree[A]

  def branch[A](left: Tree[A], right: Tree[A]): Tree[A] = Branch(left, right)

  def leaf[A](value: A): Tree[A] = Leaf(value)

  import cats.Monad
  import cats.Eval
  import scala.annotation.tailrec

  implicit val treeMonad = new Monad[Tree] {
    def flatMap[A, B](fa: Tree[A])(fn: A => Tree[B]): Tree[B] = fa match {
      case Branch(l, r) => Branch(flatMap(l)(fn), flatMap(r)(fn))
      case Leaf(a) => fn(a)
    }

    def pure[A](a: A): Tree[A] = leaf(a)

    // my sol
    //@tailrec
    /*
    def tailRecM[A, B](a: A)(fn: A => Tree[Either[A, B]]): Tree[B] = {

      def aux(fa: Tree[Either[A, B]]): Tree[B] = fa match {
        case Branch(l, r) => branch(aux(l), aux(r))
        case Leaf(Left(a)) => tailRecM(a)(fn)
        case Leaf(Right(b)) => Leaf(b)
      }

      aux(fn(a))
    }
    */

    // sol1
    /*
    def tailRecM[A, B](a: A)(fn: A => Tree[Either[A, B]]): Tree[B] = fn(a) match {
      case Branch(l, r) =>
        Branch(
          flatMap(l) {
            case Left(a) => tailRecM(a)(fn)
            case Right(b) => pure(b)
          },
          flatMap(r) {
            case Left(a) => tailRecM(a)(fn)
            case Right(b) => pure(b)
          }
        )
      case Leaf(Left(a)) => tailRecM(a)(fn)
      case Leaf(Right(b)) => Leaf(b)
    }
    */

    def tailRecM[A, B](a: A)(fn: A => Tree[Either[A, B]]): Tree[B] = {
      @tailrec
      def loop(
        open: List[Tree[Either[A, B]]],
        closed: List[Tree[B]]
      ): List[Tree[B]] = open match {
        case Branch(l, r) :: next =>
          l match {
            case Branch(_, _) => loop(l :: r :: next, closed)
            case Leaf(Left(a)) => loop(fn(a) :: r :: next, closed)
            case Leaf(Right(b)) => loop(r :: next, pure(b) :: closed)
          }
        case Leaf(Left(a)) :: next => loop(fn(a) :: next, closed)
        case Leaf(Right(b)) :: next => closed match {
          case h :: t => loop(next, branch(h, pure(b)) :: t)
          case Nil => loop(next, pure(b) :: closed)
        }
        case Nil => closed
      }

      loop(List(fn(a)), Nil).head
    }
  }
}

