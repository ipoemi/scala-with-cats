package ipoemi.scalawithcats.exercise

import scala.util.Try

final case class Box[A](value: A)

object `3.5.4` {

  import cats._, cats.implicits._

  sealed trait Tree[+A]

  final case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

  final case class Leaf[A](value: A) extends Tree[A]

  object Tree {
    def leaf[A](a: A): Tree[A] = Leaf(a)

    def branch[A](left: Tree[A], right: Tree[A]): Tree[A] = Branch(left, right)

    implicit val treeFunctor = new Functor[Tree] {
      def map[A, B](fa: Tree[A])(f: A => B): Tree[B] = fa match {
        case Branch(l, r) => Branch(map(l)(f), map(r)(f))
        case Leaf(v) => Leaf(f(v))
      }
    }
  }

}

object `3.6.1.1` {

  trait Printable[A] {
    self =>
    def format(value: A): String
    def contramap[B](func: B => A) = new Printable[B] {
      def format(value: B): String = self.format(func(value))
    }
  }

  object Printable {
    def apply[A: Printable] = implicitly[Printable[A]]
  }

  def format[A](value: A)(implicit p: Printable[A]): String =
    p.format(value)

  implicit val stringPrintable =
    new Printable[String] {
      def format(value: String): String =
        "\"" + value + "\""
    }

  implicit val booleanPrintable =
    new Printable[Boolean] {
      def format(value: Boolean): String =
        if (value) "yes" else "no"
    }

  implicit def boxPrintable[A: Printable] = Printable[A].contramap[Box[A]](_.value)
}

object `3.6.2.1` {

  trait Codec[A] {
    self =>
    def encode(value: A): String
    def decode(value: String): A

    def imap[B](dec: A => B, enc: B => A): Codec[B] = new Codec[B] {
      def encode(value: B): String = self.encode(enc(value))
      def decode(value: String): B = dec(self.decode(value))
    }
  }

  def encode[A](value: A)(implicit c: Codec[A]): String =
    c.encode(value)

  def decode[A](value: String)(implicit c: Codec[A]): A =
    c.decode(value)

  implicit val stringCodec = new Codec[String] {
    def encode(value: String): String = value
    def decode(value: String): String = value
  }

  implicit val intCodec: Codec[Int] =
    stringCodec.imap(_.toInt, _.toString)

  implicit val booleanCodec: Codec[Boolean] =
    stringCodec.imap(_.toBoolean, _.toString)

  implicit val doubleCode: Codec[Double] =
    stringCodec.imap(_.toDouble, _.toString)

  implicit def boxCode[A](implicit C: Codec[A]) = C.imap[Box[A]](Box(_), _.value)
}

object Chapter03Main {
  def main(args: Array[String]): Unit = {
    import `3.5.4`._

    import cats._, cats.implicits._

    val tree: Tree[Int] = Branch(Leaf(1), Leaf(2))
    println(tree.map(_ + 1))

    import `3.6.1.1`._

    println(format(Box(true)))

    import `3.6.2.1`._

    println(encode(Box(123)))

    println(decode[Box[Int]]("123"))


    implicit val symbolSemigroup: Semigroup[Symbol] =
      Semigroup[String].imap(Symbol.apply)(_.name)

    println('a |+| 'few |+| 'words)

    val func1 = (x: Int) => x.toDouble
    val func2 = (y: Double) => y * 2
    val func3 = func1.map(func2)
  }
}


