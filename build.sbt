val specs2Ver = "3.8.6"

val commonSettings = Seq(
  version := "0.1.0",
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
  )
)

lazy val core = (project in file("core")).
  settings(
    commonSettings ++
    Seq(
      name := "shapeless-config-core",
      libraryDependencies ++= Seq(
        "org.typelevel" %% "cats" % "0.8.1",
        "com.github.benhutchison" %% "mouse" % "0.6",
        "com.chuusai" %% "shapeless" % "2.3.2",
        "org.specs2" %% "specs2-core" % specs2Ver % "test",
        "org.specs2" %% "specs2-scalacheck" % specs2Ver % "test"
      )
    )
  )

lazy val macros = (project in file("macros")).
  settings(
    commonSettings ++
    Seq(
      name := "shapeless-config-macros",
      libraryDependencies ++= Seq(
        "org.specs2" %% "specs2-core" % specs2Ver % "test"
      )
    )
  ).dependsOn(core)

lazy val examples = (project in file("examples")).settings(commonSettings).dependsOn(macros)

lazy val root = (project in file(".")).
  settings(
    commonSettings ++ Seq(name := "shapeless-config")
  ).dependsOn(core, macros)