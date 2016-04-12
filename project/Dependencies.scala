import sbt._

object Dependencies {
  val catsVersion = "0.4.1"
  val playVersion = "2.5.1"

  val cats = "org.typelevel" %% "cats" % catsVersion
  val playWS = "com.typesafe.play" %% "play-ws" % playVersion
}
