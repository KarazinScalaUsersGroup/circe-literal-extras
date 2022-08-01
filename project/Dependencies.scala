import sbt._

object Dependencies {

  object Version {
    val cats  = "2.8.0"
    val circe = "0.15.0-M1"
    val munit = "1.0.0-M6"
  }
  
  lazy val cats: Seq[ModuleID] = Seq(
    "org.typelevel" %% "cats-laws"
  ).map(_ % Version.cats % "test")
  
  lazy val circe: Seq[ModuleID] = Seq(
    "io.circe" %% "circe-core",
    "io.circe" %% "circe-generic",
    "io.circe" %% "circe-parser",
    "io.circe" %% "circe-numbers",
    "io.circe" %% "circe-jawn",
    "io.circe" %% "circe-pointer",
    "io.circe" %% "circe-testing"
  ).map(_ % Version.circe)

  lazy val munit: Seq[ModuleID] = Seq(
    "org.scalameta" %% "munit",
    "org.scalameta" %% "munit-scalacheck"
  ).map(_ % Version.munit % "test")
  
}
