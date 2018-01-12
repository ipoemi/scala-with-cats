package ipoemi.scalawithcats.exercise

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

import cats.Applicative
import cats.Functor
import cats.Id
import cats.instances.future._
import cats.instances.list._
import cats.syntax.traverse._
import cats.syntax.functor._

object `8` {

  trait UptimeClient[F[_]] {
    def getUptime(hostname: String): F[Int]
  }

  trait RealUptimeClient extends UptimeClient[Future] {
    def getUptime(hostname: String): Future[Int]
  }

  /*
  trait TestUptimeClient extends UptimeClient[Id] {
    def getUptime(hostname: String): Int
  }
  */

  class TestUptimeClient(hosts: Map[String, Int]) extends UptimeClient[Id] {
    def getUptime(hostname: String): Int = hosts.getOrElse(hostname, 0)
  }

  class UptimeService[F[_] : Applicative : Functor](client: UptimeClient[F]) {
    def getTotalUptime(hostnames: List[String]): F[Int] =
      hostnames.traverse(client.getUptime).map(_.sum)
  }

}
