import org.scalajs.sbtplugin.AbstractJSDep

enablePlugins(ScalaJSPlugin)

name := "ScalaFromJS"

version := "0.1.1"

scalaVersion := "2.11.8"

libraryDependencies += "org.scalatest" %%% "scalatest" % "3.0.1" % "test"

def loadJSDependencies(s: String*): Seq[AbstractJSDep] = {
  val head = ProvidedJS / s.head

  val pairs = s.drop(1) zip s
  head +: pairs.map { case (depName, prev) =>
    ProvidedJS / s"uglifyjs/lib/$depName" dependsOn prev
  }
}

jsDependencies ++= loadJSDependencies(
  "utils.js",
  "ast.js",
  "parse.js",
  "transform.js",
  "scope.js",
  "output.js",
  "compress.js"
)

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

//scalaJSModuleKind := ModuleKind.CommonJSModule

persistLauncher in Compile := true

persistLauncher in Test := false
