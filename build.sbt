enablePlugins(ScalaJSPlugin)

enablePlugins(ScalaJSBundlerPlugin)

name := "ScalaFromJS"

version := "0.1.1"

scalaVersion in ThisBuild  := "2.12.1"

scalacOptions in ThisBuild ++= Seq("-unchecked", "-feature", "-deprecation")

npmDependencies in Compile += "uglify-js" -> "2.7.5"

libraryDependencies += "org.scalatest" %%% "scalatest" % "3.0.1" % "test"

def rel(parent: File, file: File) = {
  val r = (file relativeTo parent).map(_.toString).getOrElse(file.name)
  r.replaceAllLiterally("\\", "/") // avoid backslashes in the relative paths
}

unmanagedResourceDirectories := Seq()

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

skip in packageJSDependencies := false

persistLauncher in Compile := false

persistLauncher in Test := false

lazy val rsc = (project in file("resource-objects")).
  settings(
    libraryDependencies += "org.scala-lang" % "scala-compiler" % scalaVersion.value
  )

lazy val scalaFromJS = (project in file(".")).aggregate(rsc).dependsOn(rsc)
