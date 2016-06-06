name := "Gprior"
version := "1.0"
scalaVersion := "2.11.8"
    
libraryDependencies  ++= Seq(
  "org.scalanlp" %% "breeze" % "0.12",
  "org.scalanlp" %% "breeze-natives" % "0.12",
  "org.scalanlp" %% "breeze-viz" % "0.12",
  "org.ddahl" % "rscala_2.11" % "1.0.11"//,
  //"org.scalatest" % "scalatest_2.10" % "2.0" % "test"
)

resolvers ++= Seq(
  "Sonatype Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots/",
  "Sonatype Releases" at "https://oss.sonatype.org/content/repositories/releases/"
)
