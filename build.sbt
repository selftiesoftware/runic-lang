
//enablePlugins(ScalaJSPlugin)

name := "reposcript"

version := "1.0"

scalaVersion := "2.11.7"

scalacOptions in Compile ++= Seq(
  "-Xlint",
  "-deprecation"
)

libraryDependencies ++= Seq(
  "org.scalatest" % "scalatest_2.11" % "2.2.4" % "test",
  "org.scalamock" %% "scalamock-scalatest-support" % "3.2" % "test"
)

//val project = Project("reposcript", file("."))
//  .settings(Defaults.itSettings : _*)
//  .settings(
//    version := "1.0",
//    scalaVersion := "2.11.7",
//    scalacOptions in Compile ++= Seq(
//      "-Xlint",
//      "-deprecation"
//    ),
//    testOptions in Test += Tests.Argument(TestFrameworks.ScalaTest, "-oD"),
//    libraryDependencies ++= Seq(
//      "org.scalatest" %%% "scalatest_2.11" % "2.2.4" % "test",
//      "org.scalamock" %% "scalamock-scalatest-support" % "3.2" % "test"
//    )
//  )
//  .enablePlugins(ScalaJSPlugin)
