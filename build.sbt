lazy val projectSettings = Seq(
  organization := "birchmd",
  scalaVersion := "2.12.7",
  version := "0.1.0-SNAPSHOT",
  resolvers ++= Seq(
    Resolver.sonatypeRepo("releases"),
    Resolver.sonatypeRepo("snapshots")
  ),
  scalafmtOnCompile := true
)

lazy val compilerSettings = CompilerSettings.options

lazy val cfp = (project in file("."))
  .settings(projectSettings: _*)
  .settings(CompilerSettings.options)
  .settings(
    libraryDependencies ++= Seq(
      "org.scalacheck" %% "scalacheck" % "1.13.5" % "test",
      "io.monix"       %% "monix"      % "3.0.0-RC2"
    )
  )
