name := "functional-scala-playground"
version := "0.1"
scalaVersion := "2.13.5"

val essentialEffects = project
  .in(file("modules/essential-effects"))
  .settings(libraryDependencies += "org.typelevel" %% "cats-effect" % "3.1.0")

val scalaWithCats = project
  .in(file("modules/scala-with-cats"))
  .settings(libraryDependencies += "org.typelevel" %% "cats-core" % "2.1.0")
  .settings(scalacOptions ++= Seq("-Xfatal-warnings"))

val typelevelProgramming = project
  .in(file("modules/typelevel-programming"))

val dataJuggling = project
  .in(file("modules/data-juggling"))

addCommandAlias("fmt", "all scalafmtSbt scalafmtAll")
