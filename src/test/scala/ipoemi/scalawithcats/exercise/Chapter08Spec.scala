package ipoemi.scalawithcats.exercise

import org.scalatest.{Matchers, WordSpec}

class Chapter08Spec extends WordSpec with Matchers {

  "8 Testing Asynchronous Code" should {
    import `8`._


    "must be equals for the following" in {

      val hosts = Map("host1" -> 10, "host2" -> 6)
      val client = new TestUptimeClient(hosts)
      val service = new UptimeService(client)
      val actual = service.getTotalUptime(hosts.keys.toList)
      val expected = hosts.values.sum

      actual shouldBe expected
    }

  }

}
