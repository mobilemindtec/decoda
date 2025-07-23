val sharedSettings = Seq(
  scalaVersion := "3.7.1",
  name := "decoda",
  organization := "io.decoda",
  version := "0.0.1",
  scalacOptions ++= Seq(
    "-new-syntax",
    "-Wvalue-discard",
    "-Wunused:all",
    // "-Werror",
    "-deprecation",
    "-explain"
  ),
  javacOptions ++= Seq("-source", "24", "-target", "24")
)

lazy val decoda =
  // select supported platforms
  crossProject(JSPlatform, JVMPlatform, NativePlatform)
    .crossType(CrossType.Full) // [Pure, Full, Dummy], default: CrossType.Full
    .withoutSuffixFor(JVMPlatform)
    .in(file("decoda"))
    .settings(sharedSettings)
    .jsSettings( /* ... */ ) // defined in sbt-scalajs-crossproject
    .jvmSettings(
      libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.19" % "test"
    )
    // configure Scala-Native settings
    .nativeSettings( /* ... */ ) // defined in sbt-scala-native
