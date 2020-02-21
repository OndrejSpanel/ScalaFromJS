name := "ScalaFromJS"

version := "0.3.1"

scalaVersion := "2.12.10"

assemblyJarName in assembly := name.value + ".jar"

mainClass in Compile := Some("com.github.opengrabeso.scalafromjs.CommandLine")

scalacOptions ++= Seq("-unchecked", "-feature", "-deprecation")

resolvers += Resolver.githubPackages("OpenGrabeso", "esprima-scala")

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.8" % "test"

libraryDependencies += "com.github.opengrabeso" %% "esprimascala" % "0.1.4"

libraryDependencies += "org.apache.commons" % "commons-text" % "1.2"

libraryDependencies += "org.scala-lang.modules" %% "scala-swing" % "2.1.1"

libraryDependencies += "com.fifesoft" % "rsyntaxtextarea" % "3.0.8"
