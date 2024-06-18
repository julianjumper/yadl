ThisBuild / scalaVersion := "3.4.1"
val fastparse = "com.lihaoyi" %% "fastparse" % "3.1.0"

val circeVersion = "0.14.1"
lazy val root = project
  .in(file("."))
  .settings(
    assembly / mainClass := Some("Main"),
    assembly / assemblyJarName := "yadl.jar",
    name := "yadl",
    version := "0.1.0",
    libraryDependencies += "org.scalameta" %% "munit" % "0.7.29" % Test,
    libraryDependencies += fastparse,
    libraryDependencies ++= Seq(
      "io.circe" %% "circe-core",
      "io.circe" %% "circe-generic",
      "io.circe" %% "circe-parser"
    ).map(_ % circeVersion),
      libraryDependencies += "io.circe" %% "circe-parser" % circeVersion
  )
