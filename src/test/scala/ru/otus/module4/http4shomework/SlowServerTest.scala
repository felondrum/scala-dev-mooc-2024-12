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
import scala.concurrent.duration._

class SlowServerTest extends AsyncFreeSpec with Matchers {
  private def createTestClient: IO[Client[IO]] = 
    SlowServer.make.map { server =>
      Client.fromHttpApp[IO](server.routes.orNotFound)
    }

  private def makeRequest(chunk: Int, total: Int, time: Int): IO[String] = 
    createTestClient.flatMap { client =>
      client.run(Request[IO](Method.GET, uri"/slow" / chunk / total / time)).use { response =>
        response.status shouldBe Status.Ok
        response.body.through(fs2.text.utf8.decode).compile.string
      }
    }

  "Slow server" - {
    "should return correct content size" in {
      (for {
        response <- makeRequest(10, 100, 1)
      } yield {
        response.length shouldBe 100
        response shouldBe "A" * 100
      }).unsafeToFuture()
    }

    "should handle invalid parameters" in {
      (for {
        client <- createTestClient
        response <- client.run(Request[IO](Method.GET, uri"/slow" / -1 / 100 / 1)).use { response =>
          response.status shouldBe Status.BadRequest
          response.as[String]
        }
        json <- IO.fromOption(parse(response).toOption)(
          new RuntimeException("Failed to parse response")
        )
        error <- IO.fromOption(json.hcursor.get[String]("error").toOption)(
          new RuntimeException("No error field in response")
        )
      } yield {
        error should include("Invalid parameters")
      }).unsafeToFuture()
    }

    "should respect chunk size and timing" in {
      (for {
        client <- createTestClient
        startTime <- IO.realTime
        response <- client.run(Request[IO](Method.GET, uri"/slow" / 10 / 30 / 1)).use { response =>
          response.status shouldBe Status.Ok
          response.body.through(fs2.text.utf8.decode).compile.string
        }
        endTime <- IO.realTime
        duration = (endTime - startTime).toSeconds
      } yield {
        response shouldBe "A" * 30
        duration should be >= 2L // Минимум 2 секунды (3 чанка по 1 секунде)
        duration should be <= 4L // Максимум 4 секунды с учетом накладных расходов
      }).unsafeToFuture()
    }
  }
} 