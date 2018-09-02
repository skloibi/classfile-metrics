
lazy val commonSettings = Seq(
  organization := "skloibi.cfmetrics",
  version := "0.1-SNAPSHOT",
  scalaVersion := "2.12.6"
)

lazy val root = (project in file(".")).
  settings(commonSettings: _*).
  settings(
    name := "classfile-metrics",
    mainClass in assembly := Some("skloibi.cfmetrics.CFMetrics"),

    assemblyJarName in assembly := "cfmetrics.jar",

    libraryDependencies ++= Seq(
      "org.javassist" % "javassist" % "3.23.1-GA"
    )
  )
  .enablePlugins(JvmPlugin)
