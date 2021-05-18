name := "jabberwocky"
version := "0.1.0"
scalaVersion := "2.13.6"

scalacOptions := Seq("-unchecked", "-deprecation", "-encoding", "utf8")

val AkkaVersion = "2.6.14"
libraryDependencies += "com.typesafe.akka" %% "akka-actor" % AkkaVersion
