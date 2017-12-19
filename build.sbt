import ProjectHelper._
import Dependencies._

git.baseVersion := "0.1"

Revolver.settings
//enablePlugins(net.virtualvoid.optimizer.SbtOptimizerPlugin)

name := "Scratch Pad"
scalaVersion in ThisBuild := Versions.ScalaV
scalacOptions in ThisBuild ++= Seq(
  "-target:jvm-1.8",
  "-feature",
  "-deprecation",
  "-unchecked",
  "-language:implicitConversions",
  "-language:higherKinds",
  "-language:existentials",
  "-language:postfixOps",
  "-Ypartial-unification"
)

resolvers ++= Seq(
  Resolver.sonatypeRepo("releases"),
  Resolver.sonatypeRepo("snapshots"),
 "Typesafe Snapshots" at "https://repo.typesafe.com/typesafe/snapshots/")

lazy val fpinscala = makeProject("fpinscala")
  .dependsOn(common)
  .settings(
    libraryDependencies += "org.typelevel" %% "discipline" % "0.8",
    initialCommands in console := """
        import scratchpad.ch15Streams._
        import SimpleStreamTransducers._
        import Process._"""
  )

lazy val common = project in file("common")

lazy val catsddd = makeProject("catsddd")
                    .settings(libraryDependencies++=Seq(Cats, Shapeless) ++ Monix)

lazy val ghe = makeProject("ghe")
  .settings(libraryDependencies++=Seq(Cats, Shapeless, TypeSafeConfig) ++ Monix ++ Circe ++ Sttp)

forcegc:= true
