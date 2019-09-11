name := "OneClassCollaborativeFiltering"

version := "0.1"

scalaVersion := "2.13.0"

libraryDependencies  ++= Seq(
  "org.scala-lang.modules" %% "scala-parallel-collections" % "0.2.0",

  // Last stable release
  "org.scalanlp" %% "breeze" % "1.0",

  // Native libraries are not included by default. add this if you want them (as of 0.7)
  // Native libraries greatly improve performance, but increase jar sizes.
  // It also packages various blas implementations, which have licenses that may or may not
  // be compatible with the Apache License. No GPL code, as best I know.
  "org.scalanlp" %% "breeze-natives" % "1.0"
)

resolvers += "Sonatype Releases" at "https://oss.sonatype.org/content/repositories/releases/"
