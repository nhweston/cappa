ThisBuild / organization := "com.github.nhweston"
ThisBuild / scalaVersion := "2.13.5"
ThisBuild / scalacOptions := Seq(
  "-deprecation",
  "-feature",
  "-unchecked",
  "-Xlog-implicits",
  "-language:implicitConversions",
)

lazy val root =
  (project in file(".")).settings(
    name := "cappa",
    libraryDependencies ++= Seq(
      "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.2",
      "org.scalatest" %% "scalatest" % "3.2.7" % "test",
    ),
    assemblyJarName in assembly := "cappa.jar",
    assemblyOutputPath in assembly := file(".") / "bin" / "cappa.jar",
    mainClass in assembly := Some("com.github.nhweston.cappa.Main"),
    test in assembly := (),
    assemblyMergeStrategy in assembly := {
      case PathList("META-INF", "MANIFEST.MF") => MergeStrategy.discard
      case _ => MergeStrategy.first
    }
  )
