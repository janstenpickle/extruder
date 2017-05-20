val specs2Ver = "3.8.9"

val commonSettings = Seq(
  version := "0.5.0-SNAPSHOT",
  organization := "extruder",
  scalaVersion := "2.12.2",
  crossScalaVersions := Seq("2.11.11", "2.12.2"),
  addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full),
  scalacOptions ++= Seq(
    "-unchecked",
    "-feature",
    "-deprecation:false",
    "-Xcheckinit",
    "-Xlint:-nullary-unit",
    "-Ywarn-numeric-widen",
    "-Ywarn-dead-code",
    "-Yno-adapted-args",
    "-language:_",
    "-target:jvm-1.8",
    "-encoding", "UTF-8"
  ),
  publishMavenStyle := true,
  licenses += ("MIT", url("http://opensource.org/licenses/MIT")),
  homepage := Some(url("https://github.com/janstenpickle/extruder")),
  developers := List(Developer("janstenpickle", "Chris Jansen", "janstenpickle@users.noreply.github.com", url = url("https://github.com/janstepickle"))),
  publishArtifact in Test := false,
  pomIncludeRepository := { _ => false },
  bintrayReleaseOnPublish := false,
  coverageMinimum := 90
)

lazy val core = (project in file("core")).
  settings(
    commonSettings ++
    Seq(
      name := "extruder-core",
      libraryDependencies ++= Seq(
        "org.typelevel" %% "cats" % "0.9.0",
        "com.github.benhutchison" %% "mouse" % "0.7",
        "com.chuusai" %% "shapeless" % "2.3.2",
        "org.specs2" %% "specs2-core" % specs2Ver % "test",
        "org.specs2" %% "specs2-scalacheck" % specs2Ver % "test",
        "org.typelevel" %% "discipline" % "0.7.3" % "test",
        "com.github.alexarchambault" %% "scalacheck-shapeless_1.13" % "1.1.5" % "test"
      ),
      publishArtifact in Test := true,
      coverageEnabled := true
    )
  )

lazy val systemSources = (project in file("system-sources")).
  settings (
    commonSettings ++ Seq(name := "extruder-system-sources")
  ).dependsOn(core)

lazy val examples = (project in file("examples")).
  settings (
    commonSettings ++
    Seq(
      name := "extruder-examples",
      libraryDependencies ++= Seq("org.zalando" %% "grafter" % "1.4.8"),
      publishArtifact := false
    )
  ).dependsOn(systemSources, typesafe, refined)

lazy val typesafe = (project in file("typesafe")).
  settings (
    commonSettings ++
    Seq(
      name := "extruder-typesafe",
      libraryDependencies ++= Seq(
        "com.typesafe" % "config" % "1.3.1",
        "org.specs2" %% "specs2-core" % specs2Ver % "test",
        "org.specs2" %% "specs2-scalacheck" % specs2Ver % "test"
      ),
      coverageEnabled := true
    )
  ).dependsOn(core % "compile->compile;test->test")

lazy val refined = (project in file("refined")).
  settings (
    commonSettings ++
      Seq(
        name := "extruder-refined",
        libraryDependencies ++= Seq(
          "eu.timepit" %% "refined" % "0.8.1",
          "eu.timepit" %% "refined-scalacheck" % "0.8.1",
          "org.specs2" %% "specs2-core" % specs2Ver % "test",
          "org.specs2" %% "specs2-scalacheck" % specs2Ver % "test"
        ),
        coverageEnabled := true
      )
  ).dependsOn(core % "compile->compile;test->test")

lazy val root = (project in file(".")).
  settings(
    commonSettings ++
    Seq(
      name := "extruder",
      unmanagedSourceDirectories in Compile := unmanagedSourceDirectories.all(aggregateCompile).value.flatten,
      sources in Compile  := sources.all(aggregateCompile).value.flatten,
      libraryDependencies := libraryDependencies.all(aggregateCompile).value.flatten
    )
  ).aggregate(core, typesafe, refined)

lazy val aggregateCompile =
  ScopeFilter(
    inProjects(core, systemSources),
    inConfigurations(Compile)
  )
