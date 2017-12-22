name := "ScalaFromJS"

version := "0.2.1"

scalaVersion := "2.11.11"

scalacOptions ++= Seq("-unchecked", "-feature", "-deprecation")

resolvers += Resolver.sonatypeRepo("releases")

resolvers += Resolver.sonatypeRepo("snapshots")

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.4" % "test"

libraryDependencies += "com.github.opengrabeso" %% "esprimascala" % "0.0.1-SNAPSHOT"

libraryDependencies += "org.scalafx" %% "scalafx" % "8.0.144-R12"

libraryDependencies += "org.apache.commons" % "commons-text" % "1.2"
