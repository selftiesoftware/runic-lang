
name := "runic"
version := "0.2"
organization := "com.repocad"
scalaVersion := "2.12.1"
homepage := Some(url("http://repocad.com"))

scalacOptions in Compile ++= Seq(
  "-feature",
  "-Xlint",
  "-deprecation"
)

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "3.0.1" % Test,
  "org.scalamock" %% "scalamock-scalatest-support" % "3.5.0" % Test,
  "org.scalacheck" %%% "scalacheck" % "1.13.4" % "test",
  "org.scala-js" %% "scalajs-stubs" % "0.6.14" % "provided"
)

publishMavenStyle := true

publishTo := {
  val nexus = "https://oss.sonatype.org/"
  if (isSnapshot.value)
    Some("snapshots" at nexus + "content/repositories/snapshots")
  else
    Some("releases" at nexus + "service/local/staging/deploy/maven2")
}

publishArtifact in Test := false

pomIncludeRepository := { _ => false }

pomExtra := (
  <url>http://repocad.com</url>
    <licenses>
      <license>
        <name>GPLv3</name>
        <url>http://www.opensource.org/licenses/GPL-3.0</url>
        <distribution>repo</distribution>
      </license>
    </licenses>
    <scm>
      <url>git@github.com:selftiesoftware/runic-lang</url>
      <connection>scm:git:git@github.com:selftiesoftware/runic-lang.git</connection>
    </scm>
    <developers>
      <developer>
        <id>jegp</id>
        <name>Jens Egholm Pedersen</name>
        <url>http://github.com/Jegp</url>
      </developer>
    </developers>)

enablePlugins(ScalaJSPlugin)
