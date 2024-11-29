ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.3.1"

lazy val root = (project in file("."))
  .settings(
    name := "PSA2 project"
  )
libraryDependencies += "org.scalafx" % "scalafx_3" % "20.0.0-R31"

val circeVersion = "0.14.1"

libraryDependencies ++= Seq(
  "io.circe" %% "circe-core",
  "io.circe" %% "circe-generic",
  "io.circe" %% "circe-parser",
  "io.circe" %% "circe-yaml"
).map(_ % circeVersion)

libraryDependencies += "org.scala-lang" %% "toolkit" % "0.1.7"


libraryDependencies += "com.lihaoyi" %% "upickle" % "3.1.0"


libraryDependencies += "com.lihaoyi" %% "requests" % "0.8.0"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.18" % "test"

