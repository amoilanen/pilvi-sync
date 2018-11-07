name := "pilvi-sync"

version := "0.1"

scalaVersion := "2.12.7"

libraryDependencies ++= Seq(
  "com.google.api-client" % "google-api-client" % "1.23.0",
  "com.google.oauth-client" % "google-oauth-client-jetty" % "1.23.0",
  "com.google.apis" % "google-api-services-drive" % "v3-rev110-1.23.0"
)
