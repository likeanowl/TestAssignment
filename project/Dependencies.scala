import sbt._

object Dependencies {
  val catsCore = "org.typelevel" %% "cats-core" % "2.1.0"
  val circeCore = "io.circe" % "circe-core_2.13" % "0.14.0-M1"
  val circeParser = "io.circe" % "circe-parser_2.13" % "0.14.0-M1"

  lazy val compileDependencies =
    Seq(catsCore, circeCore, circeParser)

  val scalatestCore = "org.scalatest" % "scalatest-core_2.13" % "3.2.0-M4" % Test
  // https://mvnrepository.com/artifact/org.scalatest/scalatest-flatspec
  val scalatestFlatspec = "org.scalatest" % "scalatest-flatspec_2.13" % "3.2.0-M4" % Test
  // https://mvnrepository.com/artifact/org.scalatest/scalatest-matchers-core
  val scalatestMatchers = "org.scalatest" % "scalatest-matchers-core_2.13" % "3.2.0-M4" % Test

  lazy val testDependencies =
    Seq(scalatestCore, scalatestFlatspec, scalatestMatchers)
}