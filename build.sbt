name := "ScalaFromJS"

version := "0.3.0"

scalaVersion := "2.12.9"

assemblyJarName in assembly := name.value + ".jar"

mainClass in Compile := Some("com.github.opengrabeso.scalafromjs.CommandLine")

scalacOptions ++= Seq("-unchecked", "-feature", "-deprecation")

resolvers += Resolver.sonatypeRepo("releases")

resolvers += Resolver.sonatypeRepo("snapshots")

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.4" % "test"

libraryDependencies += "com.github.opengrabeso" %% "esprimascala" % "0.0.1-SNAPSHOT"

libraryDependencies += "org.apache.commons" % "commons-text" % "1.2"

libraryDependencies += "org.scala-lang.modules" %% "scala-swing" % "2.1.1"
