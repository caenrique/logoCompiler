lazy val root = (project in file(".")).
  settings(
    name := "logo-compiler",
    version := "1.0",
    scalaVersion := "2.13.0",
    mainClass in Compile := Some("cli.Main")
  )

libraryDependencies ++= Seq(
"org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.2"
)
