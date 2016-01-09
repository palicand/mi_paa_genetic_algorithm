name := "KnapsackEvA"

version := "1.0"

scalaVersion := "2.11.7"
libraryDependencies += "org.scalatest" % "scalatest_2.11" % "2.2.4" % "test"
libraryDependencies += "ch.qos.logback" % "logback-classic" % "1.1.3"
libraryDependencies += "com.typesafe.scala-logging" %% "scala-logging" % "3.1.0"


scalacOptions ++= Seq("-unchecked", "-deprecation", "–Xfatal-warnings", "–Xlint")