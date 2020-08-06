import sbt._

object Dependencies
{
  val SCALA_VERSION = "2.13.3"

  lazy val shapeless = "com.chuusai" %% "shapeless" % "2.3.3"

  lazy val kindProjector = "org.typelevel" %% "kind-projector" % "0.11.0" cross CrossVersion.full

  lazy val scalaTypedHoles = "com.github.cb372" % "scala-typed-holes" % "0.1.5" cross CrossVersion.full

  lazy val scalaTest = "org.scalatest" %% "scalatest" % "3.2.0"

  lazy val pegdown = "org.pegdown" % "pegdown" % "1.6.0"
}
