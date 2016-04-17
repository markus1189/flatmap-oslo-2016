import sbt._
import Keys._

object MyProject extends Build {
  import Dependencies._

  lazy val flatmap = Project("flatmap-oslo", file("."))
    .settings(
      scalaVersion := "2.11.8",
      libraryDependencies ++= Seq(cats, playWS, scalatest),
      fork := true,
      resolvers += Resolver.sonatypeRepo("releases"),
      addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.7.1"),
      scalacOptions ++= Seq(
        "-feature",
        "-language:higherKinds",
        "-Xlint",
        "-Ywarn-infer-any",
        "-Ywarn-unused"
      )
    )
}
