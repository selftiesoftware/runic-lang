
name := "reposcript"

version := "1.0"

scalaVersion := "2.11.7"

scalacOptions in Compile ++= Seq(
  "-Xlint",
  "-deprecation"
)

libraryDependencies ++= Seq(
  "org.scalatest" %%% "scalatest" % "3.0.0-M15" % Test,
  "org.scalamock" %%% "scalamock-scalatest-support" % "3.2" % Test
)

//enablePlugins(ScalaJSPlugin)
