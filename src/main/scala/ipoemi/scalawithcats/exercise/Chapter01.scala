package ipoemi.scalawithcats.exercise

object `1.3` {

  trait Printable[A] {
    def format(a: A): String
  }

  object PrintableInstances {
    val intPrintableInstance = new Printable[Int] {
      def format(a: Int): String = a.toString
    }
    val stringPrintableInstance = new Printable[String] {
      def format(a: String): String = a
    }
  }

  object Printable {
    def apply[A](implicit A: Printable[A]) = A
    def format[A: Printable](a: A): String = Printable[A].format(a)
    def print[A: Printable](a: A): Unit = println(Printable[A].format(a))
  }

  final case class Cat(name: String, age: Int, color: String)

  case object Cat {
    implicit val catPrintableInstance = new Printable[Cat] {
      def format(a: Cat): String =
        s"${a.name} is a ${a.age} year-old ${a.color} cat."
    }
  }

  object PrintableSyntax {

    implicit class PrintOps[A](a: A) {
      def format(implicit p: Printable[A]): String = p.format(a)
      def print(implicit p: Printable[A]): Unit = println(p.format(a))
    }

  }

}

object `1.4.6` {

  import cats._, cats.implicits._

  final case class Cat(name: String, age: Int, color: String)

  implicit val catShowInstance = Show.show[Cat] { a =>
    s"${a.name} is a ${a.age} year-old ${a.color} cat."
  }
}

object `1.5.5` {

  import cats._, cats.implicits._

  final case class Cat(name: String, age: Int, color: String)

  implicit val catEqInstance = Eq.instance[Cat] { (cat1, cat2) =>
    cat1.name === cat2.name &&
      cat1.age === cat2.age &&
      cat1.color === cat2.color
  }
}
