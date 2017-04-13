enablePlugins(ScalaJSPlugin)

enablePlugins(BuildInfoPlugin)

name := "ScalaFromJS"

version := "0.1.1"

scalaVersion in ThisBuild  := "2.12.1"

scalacOptions in ThisBuild ++= Seq("-unchecked", "-feature", "-deprecation")

jsDependencies += ProvidedJS / "uglifyjs.js"

jsDependencies += ProvidedJS / "debug.js" dependsOn "uglifyjs.js"

libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "0.9.1"

libraryDependencies += "org.scalatest" %%% "scalatest" % "3.0.1" % "test"

buildInfoOptions += BuildInfoOption.BuildTime

cancelable in Global := true

def generateIndexTask(index: String, suffix: String) = Def.task {
  val source = baseDirectory.value / "index-template.html"
  val target = (crossTarget in Compile).value / index
  val log = streams.value.log
  IO.writeLines(target,
    IO.readLines(source).map {
      line => line.replace("{{opt}}", suffix)
    }
  )

  log.info(s"Generate $index with suffix $suffix")
  target
}

Seq(
  (fastOptJS in Compile, "index-dev.html", "fastOpt"),
  (fullOptJS in Compile, "index.html", "opt")
).map {
  case (task, indexHtml, postfix) =>
    task := task.dependsOn(generateIndexTask(indexHtml, postfix)).value
}

import complete.DefaultParsers._

lazy val runa = inputKey[Unit]("Run app with arguments")

runa := {
  (fastOptJS in Compile).value // build it first
  val args: Seq[String] = spaceDelimited("<arg>").parsed
  val npmRun = "node index.js" + args.map("\"" + _ + "\"").mkString(" "," ","")
  npmRun.!
}

skip in packageJSDependencies := false

scalaJSUseMainModuleInitializer := true

scalaJSModuleKind := ModuleKind.CommonJSModule

//scalaJSOptimizerOptions in fastOptJS ~= { _.withDisableOptimizer(true) }


lazy val deployTask = TaskKey[Unit]("deploy")

deployTask := {
  //(compile in scalaFromJS).value
  (fullOptJS in Compile).value // build it first

  val binVersion = scalaBinaryVersion.value
  val baseName = name.value.toLowerCase
  val buildDir = (target in Compile).value / ("scala-" + binVersion)
  val deployDir = baseDirectory.value / "docs" / "live"
  val toDeploy = Seq("%-jsdeps.js", "%-opt.js", "index.html")
  for (suffix <- toDeploy)  {
    val fullName = suffix.replace("%", baseName)
    val builtFile = buildDir / fullName
    val deployFile = deployDir / fullName
    IO.copyFile(builtFile, deployFile)
    println(s"Deployed $builtFile to $deployFile")
  }
}


