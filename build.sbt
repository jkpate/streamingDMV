import AssemblyKeys._ // put this at the top of the file

assemblySettings

jarName in assembly := "streamingDMVAD.jar"

test in assembly := {}

name := "streamingDMV"

version := "0.001-SNAPSHOT"

scalaVersion := "2.11.7"
// scalaVersion := "2.11.6"

unmanagedClasspath in Compile += Attributed.blank(new java.io.File("doesnotexist"))

// traceLevel := 100

scalacOptions ++= Seq( "-deprecation", "-feature" )

resolvers ++= Seq(
  "Sonatype Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots/"
)

libraryDependencies  ++= Seq(
  "net.sf.jopt-simple" % "jopt-simple" % "4.0-beta1",
  "junit" % "junit" % "4.8" % "test",
  "org.scalatest" % "scalatest_2.10" % "1.9.1" % "test",
  // "org.scalanlp" %% "breeze" % "0.11.2",
  // "org.scalanlp" %% "breeze-natives" % "0.11.2"
  "org.scalanlp" %% "breeze" % "0.12-SNAPSHOT",
  "org.scalanlp" %% "breeze-natives" % "0.12-SNAPSHOT",
  "net.jpountz.lz4" % "lz4" % "1.3.0"
  // "com.twitter" %% "algebird-core" % "0.11.0"
)


testOptions in Test += Tests.Argument("-oDF")

publishArtifact in packageDoc := false

// scalaxy/streams optimization

// scalaVersion := "2.11.6"

scalacOptions ++= Seq( "-Xplugin-require:scalaxy-streams", "-J-Dscalaxy.streams.strategy=safer" )

scalacOptions in Test ~= (_ filterNot (_ == "-Xplugin-require:scalaxy-streams"))

scalacOptions in Test += "-Xplugin-disable:scalaxy-streams"

autoCompilerPlugins := true

addCompilerPlugin("com.nativelibs4java" %% "scalaxy-streams" % "0.3.4")

scalacOptions ++= Seq( "-optimise", "-Yclosure-elim"  )//, "-Yinline", "-Yinline-warnings" )

