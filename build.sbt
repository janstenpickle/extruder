import microsites._
import sbt.Keys.libraryDependencies

val catsVer = "1.5.0"
val catsEffectVer = "1.0.0"
val prometheusVer = "0.4.0"
val refinedVer = "0.9.2"
val scalaCheckVer = "1.13.5"
val scalaCheckShapelessVer = "1.1.8"
val scalaTestVer = "3.0.5"

val commonSettings = Seq(
  organization := "extruder",
  scalaVersion := "2.12.7",
  crossScalaVersions := Seq("2.11.12", "2.12.7"),
  addCompilerPlugin(("org.spire-math"  % "kind-projector" % "0.9.8").cross(CrossVersion.binary)),
  addCompilerPlugin(("org.scalamacros" % "paradise"       % "2.1.0").cross(CrossVersion.full)),
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
    "-encoding",
    "UTF-8"
  ),
  publishMavenStyle := true,
  licenses += ("MIT", url("http://opensource.org/licenses/MIT")),
  homepage := Some(url("https://github.com/janstenpickle/extruder")),
  developers := List(
    Developer(
      "janstenpickle",
      "Chris Jansen",
      "janstenpickle@users.noreply.github.com",
      url = url("https://github.com/janstepickle")
    )
  ),
  publishArtifact in Test := false,
  pomIncludeRepository := { _ =>
    false
  },
  bintrayReleaseOnPublish := true,
  coverageMinimum := 80,
  releaseCrossBuild := true,
  scalafmtOnCompile := true,
  scalafmtTestOnCompile := true,
  releaseIgnoreUntrackedFiles := true,
  parallelExecution in ThisBuild := true,
  logBuffered in Test := false
)

lazy val core = (project in file("core")).settings(
  commonSettings ++
    Seq(
      name := "extruder-core",
      libraryDependencies ++= Seq(
        "io.estatico" %% "newtype" % "0.4.2",
        ("org.typelevel" %% "cats-core" % catsVer).exclude("org.scalacheck", "scalacheck"),
        ("org.typelevel" %% "mouse"     % "0.18").exclude("org.scalacheck", "scalacheck"),
        ("com.chuusai"   %% "shapeless" % "2.3.3").exclude("org.scalacheck", "scalacheck"),
        "org.scalatest" %% "scalatest" % scalaTestVer % Test,
        ("org.typelevel" %% "cats-effect-laws" % catsEffectVer).exclude("org.scalacheck", "scalacheck"),
        ("org.typelevel" %% "discipline"       % "0.10.0" % Test)
          .exclude("org.scalacheck", "scalacheck"),
        ("com.github.alexarchambault" %% "scalacheck-shapeless_1.13" % scalaCheckShapelessVer % Test)
          .exclude("org.scalacheck", "scalacheck")
      ),
      publishArtifact in Test := true,
      coverageEnabled.in(Test, test) := true
    )
)

lazy val catsEffect = (project in file("cats-effect"))
  .settings(
    commonSettings ++
      Seq(
        name := "extruder-cats-effect",
        libraryDependencies ++= Seq(
          ("org.typelevel" %% "cats-effect" % catsEffectVer).exclude("org.scalacheck", "scalacheck"),
          "org.scalatest" %% "scalatest" % scalaTestVer % Test,
          ("org.typelevel" %% "cats-effect-laws" % catsEffectVer).exclude("org.scalacheck", "scalacheck"),
          ("org.typelevel" %% "discipline"       % "0.10.0" % Test)
            .exclude("org.scalacheck", "scalacheck")
        )
      )
  )
  .dependsOn(core)

lazy val systemSources = (project in file("system-sources"))
  .settings(commonSettings ++ Seq(name := "extruder-system-sources"))
  .dependsOn(core, catsEffect)

lazy val examples = (project in file("examples"))
  .settings(
    commonSettings ++
      Seq(name := "extruder-examples", publishArtifact := false)
  )
  .dependsOn(systemSources, typesafe, refined)

lazy val aws = (project in file("aws"))
  .settings(
    commonSettings ++
      Seq(
        name := "extruder-aws",
        libraryDependencies ++= Seq(
          "com.amazonaws"  % "aws-java-sdk-core"   % "1.11.354",
          "org.scalatest"  %% "scalatest"          % scalaTestVer % Test,
          "org.scalacheck" %% "scalacheck"         % scalaCheckVer % Test,
          "eu.timepit"     %% "refined-scalacheck" % refinedVer % Test
        ),
        coverageEnabled.in(Test, test) := true
      )
  )
  .dependsOn(core, refined)

lazy val typesafe = (project in file("typesafe"))
  .settings(
    commonSettings ++
      Seq(
        name := "extruder-typesafe",
        libraryDependencies ++= Seq(
          "com.typesafe"   % "config"      % "1.3.3",
          "org.scalatest"  %% "scalatest"  % scalaTestVer % Test,
          "org.scalacheck" %% "scalacheck" % scalaCheckVer % Test
        ),
        coverageEnabled.in(Test, test) := true
      )
  )
  .dependsOn(core % "compile->compile;test->test", catsEffect)

lazy val metricsCore = (project in file("metrics/core"))
  .settings(
    commonSettings ++
      Seq(
        name := "extruder-metrics-core",
        libraryDependencies ++= Seq(
          "org.scalatest"              %% "scalatest"                 % scalaTestVer           % Test,
          "org.scalacheck"             %% "scalacheck"                % scalaCheckVer          % Test,
          "com.github.alexarchambault" %% "scalacheck-shapeless_1.13" % scalaCheckShapelessVer % Test,
          "com.lihaoyi"                %% "utest"                     % "0.6.3"                % Test,
          ("org.typelevel" %% "discipline" % "0.9.0" % Test)
            .exclude("org.scalacheck", "scalacheck")
        ),
        coverageEnabled.in(Test, test) := true
      )
  )
  .dependsOn(core, catsEffect)

lazy val prometheus = (project in file("metrics/prometheus"))
  .settings(
    commonSettings ++
      Seq(
        name := "extruder-metrics-prometheus",
        libraryDependencies ++= Seq(
          "io.prometheus"              % "simpleclient"               % prometheusVer,
          "io.prometheus"              % "simpleclient_pushgateway"   % prometheusVer,
          "org.scalatest"              %% "scalatest"                 % scalaTestVer % Test,
          "org.scalacheck"             %% "scalacheck"                % scalaCheckVer % Test,
          "org.mockito"                % "mockito-core"               % "2.22.0" % Test,
          "com.github.alexarchambault" %% "scalacheck-shapeless_1.13" % scalaCheckShapelessVer % Test
        ),
        coverageEnabled.in(Test, test) := true
      )
  )
  .dependsOn(metricsCore)

lazy val spectator = (project in file("metrics/spectator"))
  .settings(
    commonSettings ++
      Seq(
        name := "extruder-metrics-spectator",
        libraryDependencies ++= Seq(
          "com.netflix.spectator"      % "spectator-api"              % "0.65.1",
          "com.netflix.spectator"      % "spectator-reg-servo"        % "0.65.1" % Test,
          "org.scalatest"              %% "scalatest"                 % scalaTestVer % Test,
          "org.scalacheck"             %% "scalacheck"                % scalaCheckVer % Test,
          "com.github.alexarchambault" %% "scalacheck-shapeless_1.13" % scalaCheckShapelessVer % Test
        ),
        coverageEnabled.in(Test, test) := true
      )
  )
  .dependsOn(metricsCore)

lazy val dropwizard = (project in file("metrics/dropwizard"))
  .settings(
    commonSettings ++
      Seq(
        name := "extruder-metrics-dropwizard",
        libraryDependencies ++= Seq(
          "io.dropwizard.metrics5"     % "metrics-core"               % "5.0.0",
          "org.scalatest"              %% "scalatest"                 % scalaTestVer % Test,
          "org.scalacheck"             %% "scalacheck"                % scalaCheckVer % Test,
          "com.github.alexarchambault" %% "scalacheck-shapeless_1.13" % scalaCheckShapelessVer % Test
        ),
        coverageEnabled.in(Test, test) := true
      )
  )
  .dependsOn(metricsCore)

lazy val refined = (project in file("refined"))
  .settings(
    commonSettings ++
      Seq(
        name := "extruder-refined",
        libraryDependencies ++= Seq(
          "eu.timepit"     %% "refined"            % refinedVer,
          "eu.timepit"     %% "refined-scalacheck" % refinedVer % Test,
          "org.scalatest"  %% "scalatest"          % scalaTestVer % Test,
          "org.scalacheck" %% "scalacheck"         % scalaCheckVer % Test
        ),
        coverageEnabled.in(Test, test) := true
      )
  )
  .dependsOn(core % "compile->compile;test->test")

lazy val root = (project in file("."))
  .settings(
    commonSettings ++
      Seq(
        name := "extruder",
        unmanagedSourceDirectories in Compile := unmanagedSourceDirectories.all(aggregateCompile).value.flatten,
        sources in Compile := sources.all(aggregateCompile).value.flatten,
        libraryDependencies := libraryDependencies.all(aggregateCompile).value.flatten
      )
  )
  .aggregate(core, catsEffect, aws, typesafe, systemSources, refined, metricsCore, dropwizard, prometheus, spectator)

lazy val aggregateCompile =
  ScopeFilter(inProjects(core), inConfigurations(Compile))

lazy val docSettings = commonSettings ++ Seq(
  micrositeName := "extruder",
  micrositeDescription := "Populate Scala case classes from any data source",
  micrositeAuthor := "Chris Jansen",
  micrositeHighlightTheme := "atom-one-light",
  micrositeHomepage := "https://janstenpickle.github.io/extruder/",
  micrositeBaseUrl := "extruder",
  micrositeDocumentationUrl := "api",
  micrositeGithubOwner := "janstenpickle",
  micrositeGithubRepo := "extruder",
  micrositeExtraMdFiles := Map(file("CONTRIBUTING.md") -> ExtraMdFileConfig("contributing.md", "docs")),
  micrositeFavicons := Seq(
    MicrositeFavicon("favicon16x16.png", "16x16"),
    MicrositeFavicon("favicon32x32.png", "32x32")
  ),
  micrositePalette := Map(
    "brand-primary" -> "#009933",
    "brand-secondary" -> "#006600",
    "brand-tertiary" -> "#339933",
    "gray-dark" -> "#49494B",
    "gray" -> "#7B7B7E",
    "gray-light" -> "#E5E5E6",
    "gray-lighter" -> "#F4F3F4",
    "white-color" -> "#FFFFFF"
  ),
  micrositePushSiteWith := GitHub4s,
  micrositeGithubToken := sys.env.get("GITHUB_TOKEN"),
  micrositeGitterChannel := false,
  micrositeCDNDirectives := CdnDirectives(
    jsList = List("https://cdn.rawgit.com/knsv/mermaid/6.0.0/dist/mermaid.min.js"),
    cssList = List("https://cdn.rawgit.com/knsv/mermaid/6.0.0/dist/mermaid.css")
  ),
  addMappingsToSiteDir(mappings in (ScalaUnidoc, packageDoc), micrositeDocumentationUrl),
  ghpagesNoJekyll := false,
  scalacOptions in (ScalaUnidoc, unidoc) ++= Seq(
    "-groups",
    "-implicits",
    "-skip-packages",
    "scalaz",
    "-sourcepath",
    baseDirectory.in(LocalRootProject).value.getAbsolutePath,
    "-doc-root-content",
    (resourceDirectory.in(Compile).value / "rootdoc.txt").getAbsolutePath
  ),
//  scalacOptions ~=
//    _.filterNot(Set("-Yno-predef")),
  git.remoteRepo := "git@github.com:janstenpickle/extruder.git",
  unidocProjectFilter in (ScalaUnidoc, unidoc) :=
    inAnyProject -- inProjects(root, examples),
  includeFilter in makeSite := "*.html" | "*.css" | "*.png" | "*.jpg" | "*.gif" | "*.svg" | "*.js" | "*.swf" | "*.yml" | "*.md"
)

lazy val docs = project
  .dependsOn(core, systemSources, typesafe, refined, metricsCore)
  .settings(
    moduleName := "extruder-docs",
    name := "Extruder docs",
    publish := (()),
    publishLocal := (()),
    publishArtifact := false
  )
  .settings(docSettings)
  .enablePlugins(ScalaUnidocPlugin)
  .enablePlugins(GhpagesPlugin)
  .enablePlugins(MicrositesPlugin)
