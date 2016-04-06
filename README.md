[![Travis build status](https://travis-ci.org/repocad/reposcript.svg)](http://travis-ci.org/repocad/reposcript)
[![Gitter chat](https://badges.gitter.im/repocad/RepoCad.png)](https://gitter.im/repocad/RepoCad)

A textual language for programming designs
===
Reposcript is a programming language for designs. This library is written in [Scala](http://scala-lang.org)
and lexes, parses and evaluates scripts written in RepoScript. The evaluation step can be performed on any
surface which can draw arcs, bezier curves, lines and text. 
While the library is written in Scala it can be compiled to JavaScript via [ScalaJS](http://scala-js.org)
(as seen in [RepoCad](http://repocad.com)) or other platforms.

This software is in a very early stage. Use at your own risk.

Usage 
==
In your [sbt](http://scala-sbt.org) project, include the following in your ``build.sbt``:
````
// Add Sonatype as a resolver
resolvers += 
  "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"

libraryDependencies += "com.repocad" %% "reposcript" % "0.1-SNAPSHOT

````

License
==

Licensed under GPL. Please read the [license agreement](https://github.com/repocad/reposcript/raw/master/repocad_license_agreement.pdf) in the project root.

Contact: jensep@protonmail.com
