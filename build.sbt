name := "wind-power-forecast"
organization := "your.organization" // Change this to your own or your organization's domain
version := "1.0"
scalaVersion := "2.13.12" // Consider if you need to update this version

libraryDependencies ++= Seq(
  "org.scala-lang.modules" %% "scala-parser-combinators" % "2.3.0", // Keep if you are using it in your project
  "com.softwaremill.sttp.client3" %% "core" % "3.3.15", // STTP core for HTTP requests
  "io.circe" %% "circe-core" % "0.14.1", // Circe core for JSON handling
  "io.circe" %% "circe-generic" % "0.14.1", // Circe generic for automatic derivation of encoders/decoders
  "io.circe" %% "circe-parser" % "0.14.1", // Circe parser for parsing strings into JSON
  "com.softwaremill.sttp.client3" %% "circe" % "3.3.15" // STTP support for Circe
)
