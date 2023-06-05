import sbtassembly.AssemblyPlugin.autoImport.assemblyJarName
import sbtcrossproject.CrossPlugin.autoImport.{crossProject, CrossType}

def generateIndexTask(index: String, suffix: String) = Def.task {
  val source = baseDirectory.value / "index.html"
  val target = (Compile / crossTarget).value / index
  val log = streams.value.log
  IO.writeLines(target,
    IO.readLines(source).map {
      line => line.replace("{{target-js}}", s"scalafromjs-$suffix.js")
    }
  )

  log.info(s"Generate $index with suffix: $suffix")
}

ThisBuild / githubOwner := "OndrejSpanel"

ThisBuild / githubRepository := "ScalaFromJS"

ThisBuild / githubActor := "OndrejSpanel"

ThisBuild / githubTokenSource := TokenSource.Environment("GITHUB_USERTOKEN") || TokenSource.Environment("GITHUB_TOKEN") || TokenSource.GitConfig("github.token")

ThisBuild / resolvers += Resolver.githubPackages("OpenGrabeso", "packages")


lazy val commonSettings = Seq(
  version := "0.7.1",
  scalaVersion := "2.13.8",
  scalacOptions ++= Seq("-unchecked", "-feature", "-deprecation"),
  libraryDependencies += "com.github.opengrabeso" %%% "esprimascala" % "0.2.15",
  libraryDependencies += "org.scala-lang.modules" %%% "scala-parser-combinators" % "1.1.2",
  libraryDependencies += "org.scalatest" %%% "scalatest" % "3.2.2" % "test"
)

lazy val root = project.in(file("root")).
  aggregate(pJVM, pJS).
  settings(
    name := "ScalaFromJS",
    publish := {},
    publishLocal := {}
  )

lazy val projs = crossProject(JSPlatform, JVMPlatform).crossType(CrossType.Full).in(file("."))
  .settings(
    name := "ScalaFromJS",
    commonSettings
  )
  .jvmSettings(
    // Add JVM-specific settings here
    Compile / mainClass  := Some("com.github.opengrabeso.scalafromjs.CommandLine"),
    libraryDependencies += "org.scala-lang.modules" %% "scala-swing" % "2.1.1",
    libraryDependencies += "com.fifesoft" % "rsyntaxtextarea" % "3.0.8",
    assembly / assemblyJarName := name.value + ".jar"
  )
  .jsSettings(
    Compile / mainClass := Some("com.github.opengrabeso.scalafromjs.ScalaFromJS"),
    scalaJSUseMainModuleInitializer := true,
    libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "1.1.0",
    (Compile / fastOptJS) := (Compile / fastOptJS).dependsOn(generateIndexTask("index-fast.html","fastopt")).value,
    (Compile / fullOptJS) := (Compile / fullOptJS).dependsOn(generateIndexTask("index.html","opt")).value
  )


lazy val pJVM = projs.jvm
lazy val pJS = projs.js.disablePlugins(sbtassembly.AssemblyPlugin)

