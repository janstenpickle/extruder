import microsites.ExtraMdFileConfig

val specs2Ver = "3.8.9"

val commonSettings = Seq(
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
  coverageMinimum := 90,
  releaseCrossBuild := true
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
      coverageEnabled.in(Test, test) := true
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
      coverageEnabled.in(Test, test) := true
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
        coverageEnabled.in(Test, test) := true
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

  lazy val docSettings = commonSettings ++ Seq(
    micrositeName := "extruder",
    micrositeDescription := "Populate Scala case classes from any any configuration source",
    micrositeAuthor := "Chris Jansen",
    micrositeHighlightTheme := "atom-one-light",
    micrositeHomepage := "https://janstenpickle.github.io/extruder/",
    micrositeBaseUrl := "extruder",
    micrositeDocumentationUrl := "api",
    micrositeGithubOwner := "janstenpickle",
    micrositeGithubRepo := "extruder",
    micrositeExtraMdFiles := Map(file("CONTRIBUTING.md") -> ExtraMdFileConfig("contributing.md", "docs")),
    micrositePalette := Map(
      "brand-primary" -> "#009933",
      "brand-secondary" -> "#006600",
      "brand-tertiary" -> "#339933",
      "gray-dark" -> "#49494B",
      "gray" -> "#7B7B7E",
      "gray-light" -> "#E5E5E6",
      "gray-lighter" -> "#F4F3F4",
      "white-color" -> "#FFFFFF"),
    micrositePushSiteWith := GitHub4s,
    micrositeGithubToken := sys.env.get("GITHUB_TOKEN"),
    addMappingsToSiteDir(mappings in (ScalaUnidoc, packageDoc), micrositeDocumentationUrl),
    ghpagesNoJekyll := false,
    scalacOptions in (ScalaUnidoc, unidoc) ++= Seq(
      "-groups",
      "-implicits",
      "-skip-packages", "scalaz",
      "-sourcepath", baseDirectory.in(LocalRootProject).value.getAbsolutePath,
      "-doc-root-content", (resourceDirectory.in(Compile).value / "rootdoc.txt").getAbsolutePath
    ),
    scalacOptions ~= {
      _.filterNot(Set("-Yno-predef"))
    },
    git.remoteRepo := "git@github.com:janstenpickle/extruder.git",
    unidocProjectFilter in (ScalaUnidoc, unidoc) :=
      inAnyProject -- inProjects(root, examples),
    includeFilter in makeSite := "*.html" | "*.css" | "*.png" | "*.jpg" | "*.gif" | "*.svg" | "*.js" | "*.swf" | "*.yml" | "*.md"
  )

lazy val docs = project.dependsOn(core, systemSources, typesafe, refined)
  .settings(
    moduleName := "extruder-docs",
    name := "Extruder docs",
    publish := (),
    publishLocal := (),
    publishArtifact := false
  )
  .settings(docSettings)
  .enablePlugins(ScalaUnidocPlugin)
  .enablePlugins(GhpagesPlugin)
  .enablePlugins(MicrositesPlugin)
