enablePlugins(ScalaJSPlugin)

enablePlugins(ScalaJSBundlerPlugin)

name := "ScalaFromJS"

version := "0.1.1"

scalaVersion := "2.12.1"

npmDependencies in Compile += "uglify-js" -> "2.7.5"

libraryDependencies += "com.lihaoyi" %%% "utest" % "0.4.5" % "test"

testFrameworks += new TestFramework("utest.runner.Framework")

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

def shortestRelName(dirs: Seq[File], file: File) = {
  val parent = dirs maxBy { dir =>
    if (file.toPath.startsWith(dir.toPath)) {
      dir.length
    } else 0
  }
  val r = parent.toPath.relativize(file.toPath).toString
  r.replaceAllLiterally("\\", "/") // avoid backslashes in the relative paths
}

def dirRoots(dirs: Seq[File]): Seq[File] = {
  // list of directories contains all subdirectories as well
  // we want to get the list of the root directories only (typically this is one directory only)
  Seq(dirs.head)
}

sourceGenerators in Test += Def.task {
  val log = streams.value.log
  val sourceDirs = dirRoots((unmanagedResources in Test).value filter ( _.isDirectory ))
  log.info("SourceDirs " + sourceDirs.mkString(","))
  val sources = (unmanagedResources in Test).value filter ( _.isFile )
  val dir = (sourceManaged in Test).value
  sources map { src =>
    val symName = shortestRelName(sourceDirs, src)
    log.info("symName " + symName)
    val f = dir / (symName + ".scala")
    IO.write(f, "object `" + symName + "` {\nval str =\"\"\"" + IO.read(src) + "\"\"\"}\n")
    f
  }
}.taskValue

skip in packageJSDependencies := false

persistLauncher in Compile := false

persistLauncher in Test := false
