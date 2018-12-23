//enablePlugins(ScalaJSPlugin)
enablePlugins(ScalaJSBundlerPlugin)

lazy val server = project.settings(
  scalaJSProjects := Seq(client),
  pipelineStages in Assets := Seq(scalaJSPipeline)
).enablePlugins(SbtWeb)

lazy val client = project.enablePlugins(ScalaJSPlugin, ScalaJSWeb)

name := "FrontendLayout"

version := "0.3.0"

scalaVersion := "2.12.6"

scalaJSUseMainModuleInitializer := true

emitSourceMaps := false

libraryDependencies ++= Seq(
  "com.github.japgolly.scalajs-react" %%% "core" % "1.3.1",
  "com.github.japgolly.scalajs-react" %%% "extra" % "1.3.1",
  "com.olvind" %%% "scalajs-react-components" % "1.0.0-M2",
  "org.scala-js" %%% "scalajs-dom" % "0.9.6",
  "com.github.japgolly.scalacss" %%% "ext-react" % "0.5.3"  // HU 01122018
  //  "org.scalatest" %%% "scalatest" % "3.0.0" % Test
)

npmDependencies in Compile ++= Seq(
  "react" -> "16.5.1",
  "react-dom" -> "16.5.1",
  "snabbdom" -> "0.5.3"
)

