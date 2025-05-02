package ru.otus.module4.http4shomework

import cats.effect.{IO, IOApp, Ref, Resource}
import com.comcast.ip4s.{Host, Port}
import org.http4s.{HttpRoutes, Response, EntityBody}
import org.http4s.dsl.io._
import org.http4s.circe.CirceEntityCodec._
import io.circe.Json
import io.circe.syntax._
import org.http4s.ember.server.EmberServerBuilder
import org.http4s.server.Server
import fs2.Stream
import scala.concurrent.duration._

class SlowServer {
  val routes: HttpRoutes[IO] = HttpRoutes.of[IO] {
    case GET -> Root / "slow" / IntVar(chunk) / IntVar(total) / IntVar(time) 
      if chunk > 0 && total > 0 && time > 0 =>
      
      val content: EntityBody[IO] = Stream.eval(IO("A"))
        .repeat
        .take(total)
        .chunkN(chunk)
        .metered(time.seconds)
        .map(_.toList.mkString)
        .through(fs2.text.utf8.encode)

      IO.pure(Response[IO](Ok).withEntity(content))

    case GET -> Root / "slow" / _ / _ / _ =>
      IO.pure(Response[IO](BadRequest).withEntity(
        Json.obj("error" -> "Invalid parameters. All parameters must be positive integers".asJson)
      ))
  }

  val server: Resource[IO, Server] = for {
    s <- EmberServerBuilder
      .default[IO]
      .withPort(Port.fromInt(8080).get)
      .withHost(Host.fromString("localhost").get)
      .withHttpApp(routes.orNotFound).build
  } yield s
}

object SlowServer {
  def make: IO[SlowServer] = IO(new SlowServer)
}

object mainServiceSlow extends IOApp.Simple {
  def run(): IO[Unit] = {
    SlowServer.make.flatMap(_.server.use(_ => IO.never))
  }
} 