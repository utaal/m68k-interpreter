name := "m68k-emu"

scalaVersion := "2.9.1"

libraryDependencies ++= Seq(
  "org.specs2" %% "specs2" % "1.8.2" % "test"
, "org.specs2" %% "specs2-scalaz-core" % "6.0.1"
)

// Read here for optional dependencies: 
// http://etorreborre.github.com/specs2/guide/org.specs2.guide.Runners.html#Dependencies

resolvers ++= Seq(
  "snapshots" at "http://oss.sonatype.org/content/repositories/snapshots"
)

testOptions := Seq(Tests.Filter(s =>
  Seq("Spec", "Suite", "Unit", "all").exists(s.endsWith(_))
))
