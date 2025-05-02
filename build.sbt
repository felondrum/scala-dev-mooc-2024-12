scalaVersion := "2.13.16"


name := "scala-dev-mooc-2024-12"
organization := "ru.otus"
version := "1.0"

libraryDependencies += Dependencies.ScalaTest
libraryDependencies += Dependencies.CatsCore
libraryDependencies += Dependencies.CatsEffect
libraryDependencies ++= Dependencies.ZIO
libraryDependencies ++= Dependencies.ZioConfig
libraryDependencies ++= Dependencies.fs2
libraryDependencies ++= Dependencies.http4s
libraryDependencies ++= Dependencies.circe

libraryDependencies ++= Seq(
  "org.http4s" %% "http4s-ember-server" % "0.23.26",
  "org.http4s" %% "http4s-ember-client" % "0.23.26",
  "org.http4s" %% "http4s-circe" % "0.23.26",
  "org.http4s" %% "http4s-dsl" % "0.23.26",
  "io.circe" %% "circe-core" % "0.14.6",
  "io.circe" %% "circe-generic" % "0.14.6",
  "io.circe" %% "circe-parser" % "0.14.6",
  "org.typelevel" %% "cats-effect-testing-scalatest" % "1.5.0" % Test
)
