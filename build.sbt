import Dependencies._

lazy val root =
  (project in file("."))
    .enablePlugins(BuildInfoPlugin, JavaAppPackaging)
    .settings(
      name := "csv-encoder",
      organization := "com.ruchij",
      scalaVersion := SCALA_VERSION,
      version := "0.0.1",
      maintainer := "me@ruchij.com",
      libraryDependencies ++= rootDependencies ++ rootTestDependencies.map(_ % Test),
      buildInfoKeys := BuildInfoKey.ofN(name, organization, version, scalaVersion, sbtVersion),
      buildInfoPackage := "com.eed3si9n.ruchij",
      topLevelDirectory := None,
      scalacOptions ++= Seq("-Xlint", "-feature"),
//      scalacOptions ++= Seq("-Xlint", "-feature", "-Xprint:typer"),
      addCompilerPlugin(kindProjector),
      addCompilerPlugin(scalaTypedHoles)
    )

lazy val rootDependencies =
  Seq(shapeless)

lazy val rootTestDependencies =
  Seq(scalaTest, pegdown)

addCommandAlias("testWithCoverage", "; coverage; test; coverageReport")
