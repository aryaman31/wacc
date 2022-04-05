Global / onChangedBuildSource := ReloadOnSourceChanges

val projectName = "HOT_WACC_11"

lazy val sbtAssemblySettings = baseAssemblySettings ++ Seq(
    assembly / assemblyOutputPath := baseDirectory.value / s"$projectName.jar",
    assembly / assemblyMergeStrategy := {
        case PathList("META-INF", _*) => MergeStrategy.discard
        case _                        => MergeStrategy.first
    }
)

lazy val root = (project in file("."))
  .settings(
    name := projectName,
    scalaVersion := "2.13.8",
    version := "0.1.0",
    mainClass in assembly := Some("com.domain.Main"),
    sbtAssemblySettings,

    scalacOptions ++= Seq("-deprecation", "-unchecked", "-feature"),
    libraryDependencies += "com.github.j-mie6" %% "parsley" % "3.3.2",
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.8" % Test
  )
