import sbt._

lazy val monoidaccount = (project in file(".")).
  settings (
    name := "monoid-account",
    organization := "org.justinhj",
    version := "0.1-SNAPSHOT",
    scalaVersion := "2.12.10"
    // add other settings here
  )

/* scala versions and options */
scalaVersion := "2.12.10"

addCompilerPlugin("com.olegpy" %% "better-monadic-for" % "0.3.0")
addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.6")

// These options will be used for *all* versions.
scalacOptions ++= Seq(
  "-deprecation"
    , "-unchecked"
    , "-encoding", "UTF-8"
    , "-Xlint"
    , "-Xverify"
    , "-feature"
    ,"-Ypartial-unification"
    //  ,"-Xfatal-warnings"
    , "-language:_"
    //,"-optimise"
    //,"-Xlog-implicit-conversions"
)

javacOptions ++= Seq("-Xlint:unchecked", "-Xlint:deprecation", "-source", "1.7", "-target", "1.7")

// javaOptions in Universal ++= Seq(
//   "-J-server",
//   "-J-Xms1g -Xmx4g",
//   "-J-XX:+UseConcMarkSweepGC -XX:+CMSParallelRemarkEnabled",
//   "-J-XX:+UseCMSInitiatingOccupancyOnly -XX:CMSInitiatingOccupancyFraction=68",
//   "-J-XX:+ScavengeBeforeFullGC -XX:+CMSScavengeBeforeRemark",
//   "-J-XX:+UseGCLogFileRotation -XX:NumberOfGCLogFiles=10 -XX:GCLogFileSize=100M"
// )

val CatsVersion = "2.0.0-M1"
val CatsEffectVersion = "1.3.0"
val ZIOVersion = "1.0.0-RC17"

libraryDependencies ++= Seq(
  "com.typesafe" % "config" % "1.3.1",
  // -- testing --
  "org.scalacheck" %% "scalacheck" % "1.13.4" % "test",
  "org.scalatest" %% "scalatest" % "3.0.1" % "test",
  // -- Logging --
  "ch.qos.logback" % "logback-classic" % "1.1.3",
  "com.typesafe.scala-logging" %% "scala-logging" % "3.9.0",
  // Cats
  "org.typelevel" %% "cats-core" % CatsVersion,
  "org.typelevel" %% "cats-free" % CatsVersion,
  "org.typelevel" %% "alleycats-core" % CatsVersion,
  "org.typelevel" %% "cats-testkit" % CatsVersion,
  "org.typelevel" %% "cats-effect" % CatsEffectVersion,
  // ZIO
  "dev.zio" %% "zio" % ZIOVersion,
  "dev.zio" %% "zio-streams" % ZIOVersion,
  // type classes
  "com.github.mpilquist" %% "simulacrum" % "0.12.0",
  // li haoyi ammonite repl embed
  "com.lihaoyi" % "ammonite" % "2.0.3" % "test" cross CrossVersion.full
)

//ivyScala := ivyScala.value map { _.copy(overrideScalaVersion = true) }

resolvers ++= Seq(
  //"Typesafe Snapshots" at "http://repo.typesafe.com/typesafe/snapshots/",
  "Typesafe Releases" at "https://repo.typesafe.com/typesafe/releases/",
  "Secured Central Repository" at "https://repo1.maven.org/maven2",
  Resolver.sonatypeRepo("snapshots")
)

// scalariform
//scalariformSettings

// ScalariformKeys.preferences := ScalariformKeys.preferences.value
//   .setPreference(AlignSingleLineCaseStatements, true)
//   .setPreference(DoubleIndentClassDeclaration, true)
//   .setPreference(IndentLocalDefs, true)
//   .setPreference(IndentPackageBlocks, true)
//   .setPreference(IndentSpaces, 2)
//   .setPreference(MultilineScaladocCommentsStartOnFirstLine, false)

// ammonite repl
sourceGenerators in Test += Def.task {
  val file = (sourceManaged in Test).value / "amm.scala"
  IO.write(file, """object amm extends App { ammonite.Main().run() }""")
  Seq(file)
}.taskValue

