name := "ScalaFromJS"

version := "0.2.1"

scalaVersion := "2.12.4"

scalacOptions ++= Seq("-unchecked", "-feature", "-deprecation")

credentials += Credentials(Path.userHome / ".ivy2" / ".credentials")

resolvers += "maven-public" at "https://www.gamatron.net/nexus/repository/maven-public/"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.4" % "test"

libraryDependencies += "net.gamatron" %% "esprimascala" % "0.0.1-SNAPSHOT"
