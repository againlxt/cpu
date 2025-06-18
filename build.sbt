// See README.md for license details.
// scalacOptions ++= Seq(
//   "-deprecation",
//   "-feature",
//   "-unchecked",
//   // "-Xfatal-warnings",
//   "-language:reflectiveCalls",
// )

// scalaVersion := "2.13.14"
// val chiselVersion = "3.6.1"
// addCompilerPlugin("edu.berkeley.cs" %% "chisel3-plugin" % chiselVersion cross CrossVersion.full)
// libraryDependencies += "edu.berkeley.cs" %% "chisel3" % chiselVersion
// libraryDependencies += "edu.berkeley.cs" %% "chiseltest" % "0.6.2"

ThisBuild / scalaVersion     := "2.13.12"
ThisBuild / version          := "0.1.0"
ThisBuild / organization     := "com.github.againlxt"

val chiselVersion = "6.5.0"

lazy val root = (project in file("."))
  .settings(
    name := "single_cycle_cpu",
    libraryDependencies ++= Seq(
      "org.chipsalliance" %% "chisel" % chiselVersion,
      "org.scalatest" %% "scalatest" % "3.2.16" % "test",
	  "edu.berkeley.cs" %% "chiseltest" % "0.6.0" % Test,
    ),
    scalacOptions ++= Seq(
      "-language:reflectiveCalls",
      "-deprecation",
      "-feature",
      "-Xcheckinit",
      "-Ymacro-annotations",
    ),
    addCompilerPlugin("org.chipsalliance" % "chisel-plugin" % chiselVersion cross CrossVersion.full),
  )
