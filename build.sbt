import AssemblyKeys._ // put this at the top of the file

assemblySettings

jarName in assembly := "streamingDMV.jar"

test in assembly := {}

name := "streamingDMV"

version := "0.001-SNAPSHOT"

scalaVersion := "2.11.2"

unmanagedClasspath in Compile += Attributed.blank(new java.io.File("doesnotexist"))

// traceLevel := 100

scalacOptions ++= Seq( "-deprecation", "-feature" )

// resolvers ++= Seq(
//   // other resolvers here
//   Resolver.mavenLocal,
//   "Scala Tools Snapshots" at "http://scala-tools.org/repo-snapshots/",
//   "Sonatype Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots/"
// )
// 
libraryDependencies  ++= Seq(
//   "junit" % "junit" % "4.8" % "test",
//   "org.scalatest" % "scalatest_2.10" % "1.9.1" % "test",
//   // "org.scalanlp" %% "breeze" % "0.10-SNAPSHOT",
//   "org.scalanlp" %% "breeze" % "0.10",
//   // "org.scalanlp" %% "breeze-natives" % "0.10-SNAPSHOT",
//   "org.scalanlp" %% "breeze-natives" % "0.10",
   "net.sf.jopt-simple" % "jopt-simple" % "4.0-beta1"//,
//   "edu.stanford.nlp" % "stanford-corenlp" % "3.5.0"
)

libraryDependencies  ++= Seq(
  "junit" % "junit" % "4.8" % "test",
  "org.scalatest" % "scalatest_2.10" % "1.9.1" % "test"
)


testOptions in Test += Tests.Argument("-oDF")

publishArtifact in packageDoc := false

