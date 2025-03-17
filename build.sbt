ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.6.4"

Compile / run / fork := true

lazy val root = (project in file("."))
  .settings(
    name := "TextEditor"
  )

libraryDependencies ++= Seq(
  "org.typelevel"          %% "cats-effect" % "3.6-623178c",
  "co.fs2"                 %% "fs2-core"    % "3.12.0-RC1",
  "com.googlecode.lanterna" % "lanterna"    % "3.2.0-alpha1",
  "org.virtuslab"          %% "scala-yaml"  % "0.3.0"
)

val log4CatsVersion = "2.7.0"

libraryDependencies ++= Seq(
  "org.typelevel" %% "log4cats-core" % log4CatsVersion, // Only if you want to Support Any Backend
  "org.typelevel" %% "log4cats-slf4j" % log4CatsVersion, // Direct Slf4j Support - Recommend
  "ch.qos.logback" % "logback-classic" % "1.5.17"
)
