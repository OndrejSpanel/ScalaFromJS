enablePlugins(ScalaJSPlugin)
enablePlugins(ScalaJSBundlerPlugin)

name := "ScalaFromJS"

version := "0.1.1"

scalaVersion := "2.11.8"

npmDependencies in Compile += "uglify-js" -> "2.7.5"

libraryDependencies += "org.scalatest" %%% "scalatest" % "3.0.1" % "test"

webpackConfigFile := Some(baseDirectory.value / "webpack.config.js")


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
}

Seq(
  (fastOptJS in Compile, "index-dev.html", "fastOpt"),
  (fullOptJS in Compile, "index.html", "opt")
).map {
  case (task, indexHtml, postfix) =>
    task := task.dependsOn(generateIndexTask(indexHtml, postfix)).value
}

skip in packageJSDependencies := false

requiresDOM in Test := true

enableReloadWorkflow := true

persistLauncher in Compile := false

persistLauncher in Test := false
