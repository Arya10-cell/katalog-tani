ThisBuild / scalaVersion := "3.3.5"

lazy val root = (project in file("."))
  .settings(
    name := "katalog-tani",
    version := "0.1.0",
    libraryDependencies += "com.lihaoyi" %% "cask" % "0.9.7",
    fork := true
  )
