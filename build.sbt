name := "functional-scala-playground"
version := "0.1"
scalaVersion := "2.13.5"

val essentialEffects = project
  .in(file("modules/essential-effects"))
  .settings(libraryDependencies += "org.typelevel" %% "cats-effect" % "3.1.0")

addCommandAlias("fmt", "all scalafmtSbt scalafmtAll")
