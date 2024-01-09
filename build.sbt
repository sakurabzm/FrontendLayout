//enablePlugins(ScalaJSPlugin)
enablePlugins(ScalaJSBundlerPlugin)

lazy val server = project.settings(
  scalaJSProjects := Seq(client),
  pipelineStages in Assets := Seq(scalaJSPipeline)
).enablePlugins(SbtWeb)

lazy val client = project.enablePlugins(ScalaJSPlugin, ScalaJSWeb)

name := "FrontendLayout"

version := "1.5.0"

scalaVersion := "2.12.6"

scalaJSUseMainModuleInitializer := true

//webpackConfigFile := Some(baseDirectory.value / "my.custom.webpack.config.js")
//mainClass in Compile := Some("de.tkip.sbpm.frontend.AppRouter")

emitSourceMaps := false

resolvers += Resolver.sonatypeRepo("releases")

val circeVersion = "0.12.1"

addCompilerPlugin(
  "org.scalamacros" % "paradise" % "2.1.1" cross CrossVersion.full
)

libraryDependencies ++= Seq(
  "io.circe" %%% "circe-core",
  "io.circe" %%% "circe-generic"
).map(_ % circeVersion)

libraryDependencies += "io.circe" %%% "circe-parser" % "0.12.1"

libraryDependencies ++= Seq(
  "com.github.japgolly.scalajs-react" %%% "core" % "1.3.1",
  "com.github.japgolly.scalajs-react" %%% "extra" % "1.3.1",
//  "com.olvind" %%% "scalajs-react-components" % "1.0.0-M2",
  "org.scala-js" %%% "scalajs-dom" % "0.9.1",
//  "io.scalajs" %%% "nodejs" % "0.5.0",
  "com.github.japgolly.scalacss" %%% "ext-react" % "0.5.3",  // HU 01122018
  //  "org.scalatest" %%% "scalatest" % "3.0.0" % Test

//  "ru.pavkin" %%% "dtc-core" % "2.1.0",
//  "ru.pavkin" %%% "dtc-moment" % "2.1.0",

  "com.flowtick" %%% "graphs-core" % "0.2.3",
  "com.flowtick" %%% "graphs-graphml" % "0.2.3",
  "net.exoego" %%% "scala-js-nodejs-v12" % "0.9.0"
//  "io.scalajs.npm" %%% "filed" % "0.4.2"
//  "com.lihaoyi" % "ammonite" % "1.8.1" % "test" cross CrossVersion.full
)


//sourceGenerators in Test += Def.task {
//  val file = (sourceManaged in Test).value / "amm.scala"
//  IO.write(file, """object amm extends App { ammonite.Main.main(args) }""")
//  Seq(file)
//}.taskValue

//(fullClasspath in Test) ++= {
//  (updateClassifiers in Test).value
//    .configurations
//    .find(_.configuration == Test.name)
//    .get
//    .modules
//    .flatMap(_.artifacts)
//    .collect{case (a, f) if a.classifier == Some("sources") => f}
//}

npmDependencies in Compile ++= Seq(
  "react" -> "16.5.1",
  "react-dom" -> "16.5.1",
  "snabbdom" -> "0.5.3"
)

