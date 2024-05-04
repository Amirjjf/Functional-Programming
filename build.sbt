name := "wind-power-forecast"
organization := "your.organization" // Change this to your own or your organization's domain
version := "1.0"
scalaVersion := "2.13.12" // Consider if you need to update this version

libraryDependencies ++= Seq(
  "org.scala-lang.modules" %% "scala-parser-combinators" % "2.3.0",
  "com.softwaremill.sttp.client3" %% "core" % "3.3.15",
  "io.circe" %% "circe-core" % "0.14.1",
  "io.circe" %% "circe-generic" % "0.14.1",
  "io.circe" %% "circe-parser" % "0.14.1",
  "com.softwaremill.sttp.client3" %% "circe" % "3.3.15",
  "org.jfree" % "jfreechart" % "1.5.3"  // JFreeChart for plotting
)

