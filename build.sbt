import Dependencies._

lazy val root = project
  .in(file("."))
  .settings(name := "circe-literal-extras")
  .settings(commonSettings: _*)
  .settings(libraryDependencies ++= dependencies)
  .settings(testFrameworks += new TestFramework("munit.Framework"))
  .settings(Test / parallelExecution := true)

lazy val dependencies = circe ++ munit

testFrameworks += new TestFramework("munit.Framework")

lazy val commonSettings = Seq(
    scalaVersion     := "3.0.0",
    organization     := "group.scala.karazin",
    organizationName := "Karazin Scala Users Group",
    version          := "0.1.0",
    scalacOptions ++= Seq(
        "-deprecation",
        "-unchecked",
        "-Xfatal-warnings",
        "-Xprint-suspension",
        "-language:postfixOps",
        "-language:implicitConversions",
        "-language:higherKinds",    
    ),
    javacOptions ++= Seq(
        "-source", "11",
        "-target", "11"
    ),
)
