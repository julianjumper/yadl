ThisBuild / scalaVersion := "3.4.1"
val fastparse = "com.lihaoyi" %% "fastparse" % "3.1.0"

lazy val root = project
  .in(file("."))
  .settings(
    assembly / mainClass := Some("Main"),
    assembly / assemblyJarName := "yadl.jar",
    name := "yadl",
    version := "0.1.0",
    libraryDependencies += "org.scalameta" %% "munit" % "0.7.29" % Test,
    libraryDependencies += fastparse
  )
