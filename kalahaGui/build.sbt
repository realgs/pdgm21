name := "test"

version := "0.1"

scalaVersion := "3.0.2"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.9" % Test

libraryDependencies += "org.scala-lang.modules" %% "scala-swing" % "3.0.0"

libraryDependencies +=
  ("com.typesafe.akka" %% "akka-actor-typed" % "2.6.18")
    .cross(CrossVersion.for3Use2_13)
libraryDependencies += "ch.qos.logback" % "logback-classic" % "1.2.9" % Runtime