import Dependencies._

ThisBuild / scalaVersion     := "2.13.1"
ThisBuild / version          := "0.1.0-SNAPSHOT"
ThisBuild / organization     := "net.sigusr"
ThisBuild / organizationName := "Sigusr"

lazy val root = (project in file("."))
  .settings(
    name := "typeclasses",
    libraryDependencies += specs2 % Test,
    scalacOptions ++= Seq(
      "-Xfatal-warnings",
      "-Yrangepos",
      "-deprecation",
      "-feature",
      "-unchecked",
      "-language:implicitConversions",
      "-language:higherKinds",
      "-language:existentials",
      "-language:postfixOps"
    )
  )