name := "konstructs-server"

version := "0.1"

scalaVersion := "2.10.4"

val akkaVersion = "2.2.4"

libraryDependencies ++= Seq(
  "com.typesafe.akka"      %% "akka-actor"    % akkaVersion,
  "commons-io"             %  "commons-io"    % "2.4",
  "io.spray"               %% "spray-json"    % "1.3.1",
  "com.sksamuel.scrimage"  %% "scrimage-core" % "1.4.2",
  "org.scalatest"          %% "scalatest"     % "2.2.1"  % "test"
)

bintrayOrganization := Some("konstructs")

fork in run := true
