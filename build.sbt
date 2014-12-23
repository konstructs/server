name := "craft-server"

version := "0.1"

scalaVersion := "2.10.4"

val akkaVersion = "2.2.4"

libraryDependencies ++= Seq(
  "com.typesafe.akka" %% "akka-actor" % akkaVersion
)
