name := "scala-functional-patterns"

version := "0.1"

scalaVersion := "2.13.3"

libraryDependencies ++= List(
  "org.typelevel" %% "cats-mtl" % "1.0.0",
  "org.typelevel" %% "cats-effect" % "2.2.0"
)

addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.11.0" cross CrossVersion.full)