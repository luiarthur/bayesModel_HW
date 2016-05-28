name := "playing"

libraryDependencies  ++= Seq(
  "org.scalanlp" %% "breeze" % "0.12",
  "org.scalanlp" %% "breeze-natives" % "0.12",
  "org.scalanlp" %% "breeze-viz" % "0.12",
  "org.ddahl" % "rscala_2.11" % "1.0.11" 
)


resolvers ++= Seq(
  "Sonatype Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots/",
  "Sonatype Releases" at "https://oss.sonatype.org/content/repositories/releases/"
)


scalaVersion := "2.11.8"

//exportJars := true // packages and produces a jar file
// need to use assembly.sbt to package all dependencies also...
