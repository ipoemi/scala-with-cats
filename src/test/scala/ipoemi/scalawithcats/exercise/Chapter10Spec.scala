package ipoemi.scalawithcats.exercise

import org.scalatest.{Matchers, WordSpec}

class Chapter10Spec extends WordSpec with Matchers {

  "10.3-3 Basic Combinators" should {
    import `10.3-3`._

    import cats.instances.list._

    val check1 = Check[List[String], Int] {
      case a if a > 0 => Right(a)
      case _ => Left(List("value should be great than 0"))
    }

    val check2 = Check[List[String], Int] {
      case a if a % 2 == 0 => Right(a)
      case _ => Left(List("value should be even"))
    }

    var check3 = check1.and(check2)

    "return Left(List(\"value should be great than 0\")) for the following" in {
      check1(-1) shouldBe Left(List("value should be great than 0"))
    }

    "return Left(List(\"value should be even\")) for the following" in {
      check2(1) shouldBe Left(List("value should be even"))
    }

    "return Left(List(\"value should be great than 0\", \"value should be even\")) for the following" in {
      check3(-1) shouldBe Left(List("value should be great than 0", "value should be even"))
    }

    "return Right(2) for the following" in {
      check3(2) shouldBe Right(2)
    }

  }

  "10.3-4 Basic Combinators" should {
    import `10.3-4`._

    import cats.data.Validated
    import cats.data.Validated._

    import cats.instances.list._

    val check1 = Check[List[String], Int] {
      case a if a > 0 => valid(a)
      case _ => invalid(List("value should be great than 0"))
    }

    val check2 = Check[List[String], Int] {
      case a if a % 2 == 0 => valid(a)
      case _ => invalid(List("value should be even"))
    }

    var check3 = check1.and(check2)

    "return Invalid(List(\"value should be great than 0\")) for the following" in {
      check1(-1) shouldBe Invalid(List("value should be great than 0"))
    }

    "return Invalid(List(\"value should be even\")) for the following" in {
      check2(1) shouldBe Invalid(List("value should be even"))
    }

    "return Invalid(List(\"value should be great than 0\", \"value should be even\")) for the following" in {
      check3(-1) shouldBe Invalid(List("value should be great than 0", "value should be even"))
    }

    "return Valid(2) for the following" in {
      check3(2) shouldBe Valid(2)
    }

  }

  "10.3-5 Basic Combinators" should {
    import `10.3-5`._

    import cats.data.Validated
    import cats.data.Validated._

    import cats.instances.list._

    val check1 = Check[List[String], Int] {
      case a if a > 0 => valid(a)
      case _ => invalid(List("value should be great than 0"))
    }

    val check2 = Check[List[String], Int] {
      case a if a % 2 == 0 => valid(a)
      case _ => invalid(List("value should be even"))
    }

    var check3 = check1.and(check2)
    var check4 = check1.or(check2)

    "return Invalid(List(\"value should be great than 0\")) for the following" in {
      check1(-1) shouldBe Invalid(List("value should be great than 0"))
    }

    "return Invalid(List(\"value should be even\")) for the following" in {
      check2(1) shouldBe Invalid(List("value should be even"))
    }

    "return Invalid(List(\"value should be great than 0\", \"value should be even\")) for the following" in {
      check3(-1) shouldBe Invalid(List("value should be great than 0", "value should be even"))
    }

    "return Valid(2) for the following" in {
      check3(2) shouldBe Valid(2)
    }

    "return Valid(1) for the following" in {
      check4(1) shouldBe Valid(1)
    }

    "return Valid(-2) for the following" in {
      check4(-2) shouldBe Valid(-2)
    }

  }

  "10.4.2 Checks" should {
    import `10.4.2`._

    import cats.data.Validated
    import cats.data.Validated._

    import cats.instances.list._

    val predicate1 = Predicate[List[String], Int] {
      case a if a > 0 => valid(a)
      case _ => invalid(List("value should be great than 0"))
    }

    val predicate2 = Predicate[List[String], Int] {
      case a if a % 2 == 0 => valid(a)
      case _ => invalid(List("value should be even"))
    }

    var predicate3 = predicate1.and(predicate2)
    var predicate4 = predicate1.or(predicate2)

    "return Invalid(List(\"value should be great than 0\")) for the following" in {
      predicate1(-1) shouldBe Invalid(List("value should be great than 0"))
    }

    "return Invalid(List(\"value should be even\")) for the following" in {
      predicate2(1) shouldBe Invalid(List("value should be even"))
    }

    "return Invalid(List(\"value should be great than 0\", \"value should be even\")) for the following" in {
      predicate3(-1) shouldBe Invalid(List("value should be great than 0", "value should be even"))
    }

    "return Valid(2) for the following" in {
      predicate3(2) shouldBe Valid(2)
    }

    "return Valid(1) for the following" in {
      predicate4(1) shouldBe Valid(1)
    }

    "return Valid(-2) for the following" in {
      predicate4(-2) shouldBe Valid(-2)
    }

    val check1 = Check(predicate1 and predicate2)
    val check2 = check1.map(_.toString)
    val check3 = Check(predicate1) andThen Check(predicate2)
    val check4 = for {
      x <- check1
      y <- check3
    } yield x + y

    "return Invalid(List(\"value should be great than 0\", \"value should be even\")) for the following check" in {
      check1(-1) shouldBe Invalid(List("value should be great than 0", "value should be even"))
    }

    "return Valid(2) for the following check" in {
      check1(2) shouldBe Valid(2)
    }

    "return Valid(\"2\") for the following check" in {
      check2(2) shouldBe Valid("2")
    }

    "return Invalid(List(\"value should be great than 0\")) for the following check3" in {
      check3(-1) shouldBe Invalid(List("value should be great than 0"))
    }

    "return Valid(8) for the following check4" in {
      check4(4) shouldBe Valid(8)
    }

  }

  "10.4.3 Recap" should {
    import `10.4.3`._

    import cats.data.Validated
    import cats.data.Validated._
    import cats.data.NonEmptyList
    import cats.data.NonEmptyList._
    import cats.syntax.validated._

    "return Valid(\"ipoemi\") for the following" in {
      checkUserName("ipoemi") shouldBe Valid("ipoemi")
    }

    "return Valid(\"ipoemi@github.com\") for the following" in {
      checkEmail("ipoemi@github.com") shouldBe Valid("ipoemi@github.com")
    }

    "return \"UserName must be longer than 3 characters\".invalidNel[String] for the following" in {
      checkUserName("") shouldBe "UserName must be longer than 3 characters".invalidNel[String]
    }

    "return \"UserName must be all alphanumeric characters\".invalidNel[String] for the following" in {
      checkUserName("ipoemi%@!#") shouldBe "UserName must be all alphanumeric characters".invalidNel[String]
    }

    "return long invalid message for the following" in {
      checkEmail("i%@!#") shouldBe
        NonEmptyList(
          "Email-Domain must be longer than 3 characters",
          List("Email-Domain must contain the character .")).invalid[String]
    }

    "return long long invalid message for the following" in {
      checkEmail("123@.c") shouldBe "Email-Domain must be longer than 3 characters".invalidNel[String]
    }

    "return invalid message for the following" in {
      checkUser("a", "@jj") shouldBe
        invalid(NonEmptyList(
          "UserName must be longer than 3 characters",
          List("Email-Name must be longer than 0 characters",
            "Email-Domain must be longer than 3 characters",
            "Email-Domain must contain the character .")))
    }

    "return user for the following" in {
      checkUser("ipoemi", "ipoemi@github.com") shouldBe
        Valid(("ipoemi", "ipoemi@github.com"))
    }
  }

  "10.5 Kleisli" should {
    import `10.5`._

    import cats.data.Validated
    import cats.data.Validated._
    import cats.data.NonEmptyList
    import cats.data.NonEmptyList._
    import cats.syntax.validated._
    import cats.syntax.either._
    import cats.syntax.applicative._

    "return Valid(\"ipoemi\") for the following" in {
      checkUserName("ipoemi") shouldBe Right("ipoemi")
    }

    "return Valid(\"ipoemi@github.com\") for the following" in {
      checkEmail("ipoemi@github.com") shouldBe Right("ipoemi@github.com")
    }

    "return error1 for the following" in {
      checkUserName("") shouldBe
        "UserName must be longer than 3 characters".pure[NonEmptyList].asLeft[String]
    }

    "return error2 for the following" in {
      checkUserName("ipoemi%@!#") shouldBe
        "UserName must be all alphanumeric characters".pure[NonEmptyList].asLeft[String]
    }

    "return long invalid message for the following" in {
      checkEmail("i%@!#") shouldBe
        NonEmptyList(
          "Email-Domain must be longer than 3 characters",
          List("Email-Domain must contain the character .")).asLeft[String]
    }

    "return long long invalid message for the following" in {
      checkEmail("123@.c") shouldBe
        "Email-Domain must be longer than 3 characters".pure[NonEmptyList].asLeft[String]
    }

    "return invalid message for the following" in {
      checkUser("a", "@jj") shouldBe
        invalid(NonEmptyList(
          "UserName must be longer than 3 characters",
          List("Email-Name must be longer than 0 characters")))
    }

    "return user for the following" in {
      checkUser("ipoemi", "ipoemi@github.com") shouldBe
        Valid(("ipoemi", "ipoemi@github.com"))
    }
  }

}
