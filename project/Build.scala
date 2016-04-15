import sbt._
import Keys._

object MyProject extends Build {
  import Dependencies._

  lazy val flatmap = Project("flatmap-oslo", file("."))
    .settings(
      scalaVersion := "2.11.8",
      libraryDependencies ++= Seq(cats, playWS, scalatest),
      fork := true
    )
}
