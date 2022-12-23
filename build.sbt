name := "functional-scala-playground"
version := "0.1"
scalaVersion := "2.13.8"

val `zio-from-scratch` = project
  .in(file("modules/zio-from-scratch"))

val `async-countdown-latch` = project
  .in(file("modules/async-countdown-latch"))

val `essential-effects` = project
  .in(file("modules/essential-effects"))
  .settings(libraryDependencies += "org.typelevel" %% "cats-effect" % "3.1.0")

lazy val `cats-effect-course` = project
  .in(file("modules/cats-effect-course"))
  .settings(
    scalaVersion := "2.13.9",
    libraryDependencies += "org.typelevel" %% "cats-effect" % "3.3.14"
  )

val `scala-with-cats` = project
  .in(file("modules/scala-with-cats"))
  .settings(
    scalacOptions ++= Seq("-Xfatal-warnings"),
    libraryDependencies += "org.typelevel" %% "cats-core" % "2.1.0"
  )

val `typelevel-programming` = project
  .in(file("modules/typelevel-programming"))
  .settings(libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value)

val `data-juggling` = project.in(file("modules/data-juggling"))

val foundations = project
  .in(file("modules/foundations"))
  .settings(
    scalaVersion := "2.13.3",
    scalacOptions ++= Seq(
      "-deprecation",
      "-encoding",
      "UTF-8",
      "-feature",
      "-language:higherKinds",
      "-language:implicitConversions",
      "-language:existentials",
      "-language:postfixOps",
      "-unchecked",
      "-Ywarn-value-discard" // uncomment to transform type erasure warnings into errors
    ),
    libraryDependencies ++= Seq(
      "com.nrinaudo"      %% "kantan.csv"      % "0.6.1",
      "org.scalatestplus" %% "scalacheck-1-14" % "3.1.2.0" % Test
    )
  )
  .settings(
    addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.10.3" cross CrossVersion.binary)
  )

lazy val `fp-in-scala` = project
  .in(file("modules/fp-in-scala"))
  .settings(scalaVersion := "2.13.6")

lazy val `concurrent-programming` = project
  .in(file("modules/concurrent-programming"))
  .settings(
    scalaVersion := "2.13.6",
    fork := false,
    libraryDependencies ++= Seq(
      "commons-io" % "commons-io" % "2.11.0"
    )
  )

lazy val `shapeless-guide` = project
  .in(file("modules/shapeless-guide"))
  .settings(libraryDependencies ++= Seq("com.chuusai" %% "shapeless" % "2.3.7"))

lazy val `fp-problem-solving` = project
  .in(file("modules/fp-problem-solving"))
  .settings(scalaVersion := "2.13.6")

addCommandAlias("fmt", "all scalafmtSbt scalafmtAll")
