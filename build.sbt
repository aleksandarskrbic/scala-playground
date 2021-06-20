name := "functional-scala-playground"
version := "0.1"
scalaVersion := "2.13.6"

val essentialEffects = project
  .in(file("modules/essential-effects"))
  .settings(libraryDependencies += "org.typelevel" %% "cats-effect" % "3.1.0")

val scalaWithCats = project
  .in(file("modules/scala-with-cats"))
  .settings(libraryDependencies += "org.typelevel" %% "cats-core" % "2.1.0")
  .settings(scalacOptions ++= Seq("-Xfatal-warnings"))

val typelevelProgramming = project
  .in(file("modules/typelevel-programming"))
  .settings(libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value)

val dataJuggling = project
  .in(file("modules/data-juggling"))

val foundations = project
  .in(file("modules/foundations"))
  .settings(scalaVersion := "2.13.3")
  .settings(
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
    )
  )
  .settings(
    addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.10.3" cross CrossVersion.binary)
  )
  .settings(
    libraryDependencies ++= Seq(
      "com.nrinaudo"      %% "kantan.csv"      % "0.6.1",
      "org.scalatestplus" %% "scalacheck-1-14" % "3.1.2.0" % Test
    )
  )

addCommandAlias("fmt", "all scalafmtSbt scalafmtAll")
