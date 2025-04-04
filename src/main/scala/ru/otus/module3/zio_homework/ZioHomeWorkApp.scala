package ru.otus.module3.zio_homework

import zio.{Scope, Unsafe, ZIO, ZIOAppArgs, ZIOAppDefault, ZLayer}

object ZioHomeWorkApp extends ZIOAppDefault{
  override def run: ZIO[Any with ZIOAppArgs with Scope, Any, Any] =
  Unsafe.unsafe { implicit unsafe =>
    zio.Runtime.default.unsafe.run(
      runApp
    )
  }

}
