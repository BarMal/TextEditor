ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.6.4"

Compile / run / fork := true

lazy val root = (project in file("."))
  .settings(
    name := "TextEditor"
  )

libraryDependencies ++= Seq(
  "org.typelevel"          %% "cats-effect" % "3.6-623178c",
  "co.fs2"                 %% "fs2-core"    % "3.12.0-RC2",
  "com.googlecode.lanterna" % "lanterna"    % "3.2.0-alpha1",
  "org.virtuslab"          %% "scala-yaml"  % "0.3.0",
  "org.scalatest"          %% "scalatest"   % "3.2.19" % "test"
)

val log4CatsVersion = "2.7.0"

libraryDependencies ++= Seq(
  "org.typelevel" %% "log4cats-core" % log4CatsVersion,
  "org.typelevel" %% "log4cats-slf4j" % log4CatsVersion,
  "ch.qos.logback" % "logback-classic" % "1.5.18"
)
