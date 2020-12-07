lazy val root = project
  .in(file("."))
  .settings(
    name := "adventofcode2020",
    version := "0.1.0",

    scalaVersion := "2.13.4",
    libraryDependencies += "org.typelevel" %% "cats-effect" % "2.3.0",
    libraryDependencies += "org.scalactic" %% "scalactic" % "3.2.2",
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.2" % "test",
    libraryDependencies += "org.typelevel" %% "cats-parse" % "0.1.0"

  )
