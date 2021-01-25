import sbt._

object Dependencies {
  
  object Version {
    val `scala3-compiler` = "3.0.0-M3"
    val circe             = "0.14.0-M3"
    val munit             = "0.7.20"
  }

  lazy val `scala3-compiler`: Seq[ModuleID] = Seq(
    "org.scala-lang" % "scala3-compiler_3.0.0-M3",
  ).map(_ % Version.`scala3-compiler` withSources() withJavadoc())

  lazy val circe: Seq[ModuleID] = Seq(
    "io.circe" %% "circe-core",
    "io.circe" %% "circe-generic",
    "io.circe" %% "circe-parser",
    "io.circe" %% "circe-numbers",
    "io.circe" %% "circe-jawn",
    "io.circe" %% "circe-pointer",
  ).map(_ % Version.circe withSources() withJavadoc())

  lazy val munit: Seq[ModuleID] = Seq(
    "org.scalameta" %% "munit",
    "org.scalameta" %% "munit-scalacheck",
  ).map(_ % Version.munit % "test" withSources() withJavadoc())

}
