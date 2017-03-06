enablePlugins(ScalaJSPlugin)

enablePlugins(BuildInfoPlugin)

name := "ScalaFromJS"

version := "0.1.1"

scalaVersion in ThisBuild  := "2.12.1"

scalacOptions in ThisBuild ++= Seq("-unchecked", "-feature", "-deprecation")

jsDependencies += ProvidedJS / "uglifyjs.js"

libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "0.9.1"

libraryDependencies += "org.scalatest" %%% "scalatest" % "3.0.1" % "test"

buildInfoOptions += BuildInfoOption.BuildTime

def rel(parent: File, file: File) = {
  val r = (file relativeTo parent).map(_.toString).getOrElse(file.name)
  r.replaceAllLiterally("\\", "/") // avoid backslashes in the relative paths
}

unmanagedResourceDirectories in Test := Seq()

resourceDirectory in Test := (sourceDirectory in Test).value / "pretend-no-resources"

sourceGenerators in Test += Def.task {
  val log = streams.value.log
  val sourceDir = (sourceDirectory in Test).value / "resources"
  val sources = PathFinder(sourceDir).*** filter ( _.isFile )
  val dir = (sourceManaged in Test).value
  sources.get map { src =>
    val symName = rel(sourceDir, src)
    val f = dir / (symName + ".scala")
    IO.write(f, "package resources;object `" + symName + "` {\nval str =\"\"\"" + IO.read(src) + "\"\"\"}\n")
    f
  }
}.taskValue

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



skip in packageJSDependencies := false

persistLauncher in Compile := true

persistLauncher in Test := false

lazy val rsc = (project in file("resource-objects")).
  settings(
    version := "0.1.0",
    libraryDependencies += "org.scala-lang" % "scala-compiler" % scalaVersion.value
  )

lazy val scalaFromJS = (project in file(".")).aggregate(rsc).dependsOn(rsc)


lazy val deployTask = TaskKey[Unit]("deploy")

deployTask := {
  //(compile in scalaFromJS).value
  (fullOptJS in scalaFromJS in Compile).value // build it first

  val binVersion = scalaBinaryVersion.value
  val baseName = name.value.toLowerCase
  val buildDir = (target in Compile).value / ("scala-" + binVersion)
  val deployDir = baseDirectory.value / "docs" / "live"
  val toDeploy = Seq("%-jsdeps.js", "%-launcher.js", "%-opt.js", "index.html")
  for (suffix <- toDeploy)  {
    val fullName = suffix.replace("%", baseName)
    val builtFile = buildDir / fullName
    val deployFile = deployDir / fullName
    IO.copyFile(builtFile, deployFile)
    println(s"Deployed $builtFile to $deployFile")
  }
}


