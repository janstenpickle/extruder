val specs2Ver = "3.8.6"

val commonSettings = Seq(
  version := "0.0.1",
  organization := "extruder",
  scalaVersion := "2.12.1",
  crossScalaVersions := Seq("2.11.8", "2.12.1"),
  addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full),
  scalacOptions ++= Seq(
    "-unchecked",
    "-feature",
    "-deprecation:false",
    "-Xfatal-warnings",
    "-Xcheckinit",
    "-Xlint",
    "-Xlint:-nullary-unit",
    "-Ywarn-unused-import",
    "-Ywarn-numeric-widen",
    "-Ywarn-dead-code",
    "-Yno-adapted-args",
    "-language:_",
    "-target:jvm-1.8",
    "-encoding", "UTF-8"
  ),
  publishMavenStyle := true,
  licenses += ("MIT license", url("http://opensource.org/licenses/MIT")),
  homepage := Some(url("https://github.com/janstenpickle/extruder")),
  developers := List(Developer("janstenpickle", "Chris Jansen", "janstenpickle@users.noreply.github.com", url = url("https://github.com/janstepickle"))),
  publishArtifact in Test := false,
  pomIncludeRepository := { _ => false },
  bintrayReleaseOnPublish := false
)

lazy val core = (project in file("core")).
  settings(
    commonSettings ++
    Seq(
      name := "extruder-core",
      libraryDependencies ++= Seq(
        "org.typelevel" %% "cats" % "0.8.1",
        "com.github.benhutchison" %% "mouse" % "0.6",
        "com.chuusai" %% "shapeless" % "2.3.2",
        "org.specs2" %% "specs2-core" % specs2Ver % "test",
        "org.specs2" %% "specs2-scalacheck" % specs2Ver % "test"
      ),
      publishArtifact := false
    )
  )

lazy val macros = (project in file("macros")).
  settings(
    commonSettings ++
    Seq(
      name := "extruder-macros",
      libraryDependencies ++= Seq(
        "org.typelevel" %% "macro-compat" % "1.1.1",
        "org.specs2" %% "specs2-core" % specs2Ver % "test"
      ),
      publishArtifact := false
    )
  ).dependsOn(core)

lazy val examples = (project in file("examples")).
  settings (
    commonSettings ++
    Seq(
      name := "extruder-examples",
      libraryDependencies += "org.zalando" %% "grafter" % "1.3.1",
      publishArtifact := false
    )
  ).dependsOn(macros)

lazy val root = (project in file(".")).
  settings(
    commonSettings ++
    Seq(
      name := "extruder",
      unmanagedSourceDirectories in Compile := unmanagedSourceDirectories.all(aggregateCompile).value.flatten,
      sources in Compile  := sources.all(aggregateCompile).value.flatten,
      libraryDependencies := libraryDependencies.all(aggregateCompile).value.flatten
    )
  ).aggregate(core, macros)

lazy val aggregateCompile =
  ScopeFilter(
    inProjects(core, macros),
    inConfigurations(Compile)
  )