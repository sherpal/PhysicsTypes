ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.5.0"

lazy val root = (project in file("."))
  .settings(
    name := "PhysicsTypes",
    libraryDependencies ++= List(
      "org.scalameta" %% "munit" % "1.0.1" % Test
    ),
    scalacOptions ++= List(
      "-source:3.5",
      "-explain",
      "-explain-types",
      "-deprecation",
      "-unchecked",
      "-Wconf:any:verbose",
      "-feature",
      // set SCALA_NON_FATAL to empty string, if you want to be risky locally. ;)
      sys.env.getOrElse("SCALA_NON_FATAL", "-Xfatal-warnings"),
      "-Wunused:unsafe-warn-patvars",
      "-Wunused:linted",
      "-language:implicitConversions"
    )
  )
