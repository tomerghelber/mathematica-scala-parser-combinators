name := "mathimatica-parser"

version := "0.1"

scalaVersion := "2.13.1"

libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.2"

libraryDependencies += "org.scalatest" %% "scalatest" %  "3.0.8" % Test
libraryDependencies += "org.scalacheck" %% "scalacheck" %  "1.14.2" % Test

crossScalaVersions := List(
  "2.12.10",
  "2.11.12"
)
