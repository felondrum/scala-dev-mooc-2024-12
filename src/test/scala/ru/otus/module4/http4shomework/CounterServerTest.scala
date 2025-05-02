package ru.otus.module4.http4shomework

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import cats.implicits.catsSyntaxParallelTraverse1
import org.http4s._
import org.http4s.implicits._
import org.http4s.client.Client
import org.scalatest.freespec.AsyncFreeSpec
import org.scalatest.matchers.should.Matchers
import io.circe.parser._

class CounterServerTest extends AsyncFreeSpec with Matchers {
  private def createTestClient: IO[Client[IO]] = 
    CounterServer.make.map { server =>
      Client.fromHttpApp[IO](server.routes.orNotFound)
    }

  private def makeRequest(client: Client[IO]): IO[Int] = 
    client.expect[String](Request[IO](Method.GET, uri"/counter"))
      .flatMap { response =>
        IO.fromOption(parse(response).toOption.flatMap(_.hcursor.get[Int]("counter").toOption))(
          new RuntimeException("Failed to parse response")
        )
      }

  "Counter server" - {
    "should increment counter sequentially" in {
      (for {
        client <- createTestClient
        counter1 <- makeRequest(client)
        counter2 <- makeRequest(client)
        counter3 <- makeRequest(client)
      } yield {
        counter1 shouldBe 1
        counter2 shouldBe 2
        counter3 shouldBe 3
      }).unsafeToFuture()
    }

    "should handle concurrent requests correctly" in {
      (for {
        client <- createTestClient
        counters <- (1 to 10).toList.parTraverse(_ => makeRequest(client))
      } yield {
        counters.sorted shouldBe (1 to 10).toList
      }).unsafeToFuture()
    }
  }
} 