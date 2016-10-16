import ReleaseTransformations._

organization := "org.konstructs"

name := "konstructs-server"

scalaVersion := "2.10.4"

licenses += ("MIT", url("http://opensource.org/licenses/MIT"))

scalacOptions ++= Seq( "-unchecked", "-deprecation", "-feature", "-language:postfixOps")

val akkaVersion = "2.2.4"

resolvers += "konstructs" at "http://dl.bintray.com/konstructs/maven"

libraryDependencies ++= Seq(
  "org.konstructs"         %  "konstructs-server-api" % "0.1.17",
  "com.typesafe.akka"      %% "akka-actor"            % akkaVersion,
  "commons-io"             %  "commons-io"            % "2.4",
  "com.google.code.gson"   %  "gson"                  % "2.6.2",
  "com.sksamuel.scrimage"  %% "scrimage-core"         % "1.4.2",
  "org.scalatest"          %% "scalatest"             % "2.2.1"  % "test"
)

bintrayOrganization := Some("konstructs")

fork in run := true

releaseProcess := Seq[ReleaseStep](
  checkSnapshotDependencies,
  inquireVersions,
  runTest,
  setReleaseVersion,
  commitReleaseVersion,
  tagRelease,
  setNextVersion,
  commitNextVersion,
  pushChanges
)
