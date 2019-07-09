val scala_version = "2.13.0"

lazy val root = (project in file(".")).
  settings(
    name := "logo-compiler",
    version := "1.0",
    scalaVersion := scala_version,
    mainClass in Compile := Some("cli.Main")
  )

libraryDependencies ++= Seq(
  "org.scala-lang" % "scala-library" % scala_version,
  "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.2"
)
