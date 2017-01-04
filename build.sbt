name := "ConfigConstructor"

version := "1.0"

val commonSettings = Seq(
  version := "1.0",
  scalaVersion := "2.12.1",
  scalacOptions ++= Seq(
    "-Xlint",
    "-Xcheckinit",
    "-Xfatal-warnings",
    "-unchecked",
    "-deprecation",
    "-feature",
    "-language:implicitConversions",
    "-language:postfixOps",
    "-language:reflectiveCalls",
    "-language:existentials",
    "-language:higherKinds",
    "-Xmax-classfile-name", "242",
    "-target:jvm-1.8"
  )
)

lazy val core = (project in file("core")).
  settings(commonSettings: _*).
  settings(
    name := "configconstructor-core",
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats" % "0.8.1",
      "com.chuusai" %% "shapeless" % "2.3.2"
    )
  ).dependsOn(model)