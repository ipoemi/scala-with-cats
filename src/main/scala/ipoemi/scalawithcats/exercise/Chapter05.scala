package ipoemi.scalawithcats.exercise

import scala.language.higherKinds
import scala.language.postfixOps

object `5.4` {

  import scala.concurrent._
  import scala.concurrent.ExecutionContext.Implicits.global
  import scala.concurrent.duration._

  import cats.data.EitherT
  import cats.instances.future._
  import cats.syntax.applicative._
  import cats.syntax.either._

  val powerLevels = Map(
    "Jazz" -> 6,
    "Bumblebee" -> 8,
    "Hot Rod" -> 10
  )

  // sol1
  /*
  type Response[A] = Future[Either[String, A]]

  def getPowerLevel(autobot: String): Response[Int] =
    Future(powerLevels.get(autobot).toRight(s"Comms error: $autobot unreachable"))

  def canSpecialMove(ally1: String, ally2: String): Response[Boolean] =
    (for {
      level1 <- EitherT(getPowerLevel(ally1))
      level2 <- EitherT(getPowerLevel(ally2))
      totalLevel = level1 + level2
    } yield totalLevel > 15).value

  def tacticalReport(ally1: String, ally2: String): String = {
    val result = Await.result(canSpecialMove(ally1, ally2), 5 seconds)
    result match {
      case Left(msg) => msg
      case Right(true) => s"$ally1 and $ally2 are ready to roll out!"
      case Right(false) => s"$ally1 and $ally2 need a recharge."
    }
  }
  */

  type Response[A] = EitherT[Future, String, A]

  def getPowerLevel(autobot: String): Response[Int] =
    powerLevels.get(autobot) match {
      case Some(level) => level.pure[Response]
      case None => EitherT.left(Future(s"$autobot unreachable"))
    }

  def canSpecialMove(ally1: String, ally2: String): Response[Boolean] =
    for {
      level1 <- getPowerLevel(ally1)
      level2 <- getPowerLevel(ally2)
      totalLevel = level1 + level2
    } yield totalLevel > 15

  def tacticalReport(ally1: String, ally2: String): String = {
    val result = Await.result(canSpecialMove(ally1, ally2).value, 5 seconds)
    result match {
      case Left(msg) => s"Comms error: $msg"
      case Right(true) => s"$ally1 and $ally2 are ready to roll out!"
      case Right(false) => s"$ally1 and $ally2 need a recharge."
    }
  }

}

