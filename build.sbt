import sbtassembly.AssemblyPlugin.autoImport.assemblyJarName
import sbtcrossproject.CrossPlugin.autoImport.{crossProject, CrossType}
import sbtcrossproject.Platform

def generateIndexTask(index: String, suffix: String) = Def.task {
  val source = baseDirectory.value / "index.html"
  val target = (crossTarget in Compile).value / index
  val log = streams.value.log
  IO.writeLines(target,
    IO.readLines(source).map {
      line => line.replace("{{target-js}}", s"scalafromjs-$suffix.js")
    }
  )

  log.info(s"Generate $index with suffix: $suffix")
}

githubOwner in ThisBuild := "OndrejSpanel"

githubRepository in ThisBuild := "ScalaFromJS"

githubActor in ThisBuild := "OndrejSpanel"

githubTokenSource in ThisBuild := TokenSource.Environment("GITHUB_USERTOKEN") || TokenSource.Environment("GITHUB_TOKEN") || TokenSource.GitConfig("github.token")

resolvers in ThisBuild += Resolver.githubPackages("OpenGrabeso", "packages")


lazy val commonSettings = Seq(
  version := "0.5.1",
  scalaVersion := "2.12.12",
  scalacOptions ++= Seq("-unchecked", "-feature", "-deprecation"),
  libraryDependencies += "com.github.opengrabeso" %%% "esprimascala" % "0.2.4",
  libraryDependencies += "org.scala-lang.modules" %%% "scala-parser-combinators" % "1.1.2",
  libraryDependencies += "org.scalatest" %%% "scalatest" % "3.2.2" % "test"
)

lazy val walkers = crossProject(JSPlatform, JVMPlatform).crossType(CrossType.Pure).settings(
  libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value, // needed for macros
  commonSettings,
  name := "walkers"
)


lazy val root = project.in(file("root")).
  aggregate(pJVM, pJS).
  settings(
    name := "ScalaFromJS",
    publish := {},
    publishLocal := {}
  )

lazy val projs = crossProject(JSPlatform, JVMPlatform).crossType(CrossType.Full).in(file(".")).dependsOn(walkers)
  .settings(
    name := "ScalaFromJS",
    commonSettings
  )
  .jvmSettings(
    // Add JVM-specific settings here
    mainClass in Compile := Some("com.github.opengrabeso.scalafromjs.CommandLine"),
    libraryDependencies += "org.scala-lang.modules" %% "scala-swing" % "2.1.1",
    libraryDependencies += "com.fifesoft" % "rsyntaxtextarea" % "3.0.8",
    assemblyJarName in assembly := name.value + ".jar"
  )
  .jsSettings(
    mainClass in Compile := Some("com.github.opengrabeso.scalafromjs.ScalaFromJS"),
    scalaJSUseMainModuleInitializer := true,
    libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "1.1.0",
    (fastOptJS in Compile) := (fastOptJS in Compile).dependsOn(generateIndexTask("index-fast.html","fastopt")).value,
    (fullOptJS in Compile) := (fullOptJS in Compile).dependsOn(generateIndexTask("index.html","opt")).value
  )


lazy val pJVM = projs.jvm
lazy val pJS = projs.js.disablePlugins(sbtassembly.AssemblyPlugin)

