name := "mathimatica-parser"

scalaVersion := "2.12.8"

libraryDependencies += "org.scala-lang.modules"     %% "scala-parser-combinators" % "1.1.2"
libraryDependencies += "com.typesafe.scala-logging" %% "scala-logging"            % "3.9.2"

libraryDependencies += "ch.qos.logback" % "logback-classic" % "1.2.3"  % Test
libraryDependencies += "org.scalatest"  %% "scalatest"      % "3.0.8"  % Test
libraryDependencies += "org.scalacheck" %% "scalacheck"     % "1.14.2" % Test

// POM settings for Sonatype
organization := "com.github.tomerghelber"
homepage := Some(
  url("https://github.com/tomerghelber/mathematica-scala-parser-combinators")
)
developers := List(
  Developer(
    "tomerghelber",
    "Tomer Ghelber",
    "tomergelber@gmail.com",
    url("https://github.com/tomerghelber")
  )
)
licenses += ("gpl-3.0", url("https://www.gnu.org/licenses/gpl-3.0.html"))
