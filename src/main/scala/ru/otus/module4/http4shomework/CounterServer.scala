package ru.otus.module4.http4shomework

import cats.effect.kernel.Resource
import cats.effect.{IO, IOApp, Ref}
import com.comcast.ip4s.{Host, Port}
import org.http4s.{HttpRoutes, Response}
import org.http4s.dsl.io._
import org.http4s.circe.CirceEntityCodec._
import io.circe.Json
import io.circe.syntax._
import org.http4s.ember.server.EmberServerBuilder
import org.http4s.server.Server


class CounterServer(ref: Ref[IO, Int]) {
  val routes: HttpRoutes[IO] = HttpRoutes.of[IO] {
    case GET -> Root / "counter" =>
      for {
        count <- ref.updateAndGet(_ + 1)
        response = Json.obj("counter" -> count.asJson)
      } yield Response[IO](Ok).withEntity(response)
  }

  val server: Resource[IO, Server] = for {
    s <- EmberServerBuilder
      .default[IO]
      .withPort(Port.fromInt(8080).get)
      .withHost(Host.fromString("localhost").get)
      .withHttpApp(routes.orNotFound).build
  } yield s
}

object CounterServer {
  def make: IO[CounterServer] =
    Ref.of[IO, Int](0).map(new CounterServer(_))
}

object mainService extends IOApp.Simple {
  def run(): IO[Unit] = {
    CounterServer.make.flatMap(_.server.use(_ => IO.never))
  }


}