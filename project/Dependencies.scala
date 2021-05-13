import sbt._

object Dependencies {
  
  object Version {
    val cats  = "2.6.0"
    val circe = "0.14.0-M6"
    val munit = "0.7.25"
  }
  
  lazy val cats: Seq[ModuleID] = Seq(
    "org.typelevel" %% "cats-laws",
  ).map(_ % Version.cats % "test" withSources() withJavadoc())
  
  lazy val circe: Seq[ModuleID] = Seq(
    "io.circe" %% "circe-core",
    "io.circe" %% "circe-generic",
    "io.circe" %% "circe-parser",
    "io.circe" %% "circe-numbers",
    "io.circe" %% "circe-jawn",
    "io.circe" %% "circe-pointer"
  ).map(_ % Version.circe withSources() withJavadoc())

  lazy val munit: Seq[ModuleID] = Seq(
    "org.scalameta" %% "munit",
    "org.scalameta" %% "munit-scalacheck"
  ).map(_ % Version.munit % "test" withSources() withJavadoc())
  
}
