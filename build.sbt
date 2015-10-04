val project = Project("reposcript", file("."))
  .settings(
    version := "1.0",
    scalaVersion := "2.11.7",
    scalacOptions ++= Seq(
      "-Xlint",
      "-deprecation"
    ),
    libraryDependencies ++= Seq(
      "org.scalatest" % "scalatest_2.11" % "2.2.4" % Test,
      "org.scalamock" %% "scalamock-scalatest-support" % "3.2" % Test
    ))
  .enablePlugins(ScalaJSPlugin)
