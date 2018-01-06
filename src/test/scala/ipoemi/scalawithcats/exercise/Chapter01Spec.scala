package ipoemi.scalawithcats.exercise

import java.io.{ByteArrayOutputStream, OutputStream, PrintStream}

import org.scalatest.{Matchers, WordSpec, NonImplicitAssertions}

class Chapter01Spec extends WordSpec with Matchers with NonImplicitAssertions  {

  "1.3 Exercise: Printable Library" should {
    import `1.3`._


    "print 'Name1 is a 100 year-old Red cat.' to Console for the following" in {
      val stream = new ByteArrayOutputStream()
      val cat = Cat("Name1", 100, "Red")
      Console.withOut(stream) {
        Printable.print(cat)
      }
      stream.toString shouldBe "Name1 is a 100 year-old Red cat.\r\n"
    }

    "print 'Name1 is a 101 year-old Red cat.' to Console for the following" in {
      import Cat.catPrintableInstance
      import PrintableSyntax.PrintOps

      val stream = new ByteArrayOutputStream()
      val cat = Cat("Name1", 101, "Red")
      Console.withOut(stream) {
        cat.print
      }
      stream.toString shouldBe "Name1 is a 101 year-old Red cat.\r\n"
    }

  }

  "1.4.6 Exercise: Cat Show" should {
    import cats._, cats.implicits._
    import `1.4.6`._

    "return 'Name1 is a 100 year-old Red cat.' for the following" in {
      val cat = Cat("Name1", 100, "Red")
      Show[Cat].show(cat) shouldBe "Name1 is a 100 year-old Red cat."
    }

    "return 'Name1 is a 101 year-old Red cat.' for the following" in {
      val cat = Cat("Name1", 101, "Red")
      cat.show shouldBe "Name1 is a 101 year-old Red cat."
    }

  }

  "1.5.5 Exercise: Equality, Liberty, and Felinity" should {
    import cats._, cats.implicits._
    import `1.5.5`._

    val cat1 = Cat("Garfield", 35, "orange and black")
    val cat2 = Cat("Heathcliff", 30, "orange and black")

    val optionCat1 = Option(cat1)
    val optionCat2 = Option.empty[Cat]

    "return false equality of two cats for the following" in {
      (cat1 === cat2) shouldBe false
    }

    "return false non-equality of two cats for the following" in {
      (cat1 =!= cat2) shouldBe true
    }

    "return false equality of two option cats for the following" in {
      (optionCat1 === optionCat2) shouldBe false
    }

    "return false non-equality of two option cats for the following" in {
      (optionCat1 =!= optionCat2) shouldBe true
    }

  }

}
