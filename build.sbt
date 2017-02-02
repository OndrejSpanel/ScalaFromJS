enablePlugins(ScalaJSPlugin)

name := "ScalaFromJS"

version := "0.1.1"

scalaVersion := "2.11.8"

//jsDependencies += "org.webjars" % "esprima" % "13001.1.0-dev-harmony-fb" / "esprima.js"

//jsDependencies += "org.webjars.npm" % "estraverse" % "1.9.1" / "1.9.1/estraverse.js"

//jsDependencies += "org.webjars.npm" % "escodegen" % "1.7.0" / "1.7.0/escodegen.js"

jsDependencies += "org.webjars.npm" % "esprima" % "2.7.2" / "esprima.js"

//jsDependencies += "org.webjars.npm" % "acorn" % "4.0.3" / "acorn.js"

//jsDependencies += "org.webjars" % "uglifyjs" % "2.7.4" / "bin/uglifyjs" minified "bin/uglifyjs"


jsDependencies += "org.webjars" % "jquery" % "2.2.1" / "jquery.js" minified "jquery.min.js"

libraryDependencies += "org.scalatest" %%% "scalatest" % "3.0.1" % "test"

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
    task <<= task.dependsOn(generateIndexTask(indexHtml, postfix))
}

skip in packageJSDependencies := false

persistLauncher in Compile := true

persistLauncher in Test := false
