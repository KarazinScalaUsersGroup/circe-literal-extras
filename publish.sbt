ThisBuild / organization := "group.scala.karazin"
ThisBuild / organizationName := "Karazin Scala Users Group"
ThisBuild / organizationHomepage := Some(url("http://example.com/"))

ThisBuild / scmInfo := Some(
  ScmInfo(
    url("https://github.com/KarazinScalaUsersGroup/circe-literal-extras"),
    "scm:git@github.com:KarazinScalaUsersGroup/circe-literal-extras.git"
  )
)
ThisBuild / developers := List(
  Developer(
    id    = "identifier",
    name  = "Name",
    email = "your@email",
    url   = url("http://your.url")
  )
)

ThisBuild / description := "Some description about your project."
ThisBuild / licenses := List("Apache 2" -> new URL("http://www.apache.org/licenses/LICENSE-2.0.txt"))
ThisBuild / homepage := Some(url("https://github.com/KarazinScalaUsersGroup/circe-literal-extras"))

// Remove all additional repository other than Maven Central from POM
ThisBuild / pomIncludeRepository := { _ => false }
ThisBuild / publishTo := {
  val nexus = "https://s01.oss.sonatype.org/"
  if (isSnapshot.value) Some("snapshots" at nexus + "content/repositories/snapshots")
  else Some("releases" at nexus + "service/local/staging/deploy/maven2")
}
ThisBuild / publishMavenStyle := true
