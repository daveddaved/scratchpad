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
  "-language:postfixOps"
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

lazy val common = (project in file("common")).settings(
  makeVersionProperties := {
    val propFile = new File((resourceManaged in Compile).value, "version.properties")
    val content = "version=%s" format gitHeadCommitSha.value.getOrElse("SNAPSHOT")
    IO.write(propFile, content)
    Seq(propFile)
  },
    publishTo := Some(Resolver.file("common",  new File(Path.userHome.absolutePath+"/.m2/repository")))
)

lazy val catsddd = makeProject("catsddd")
                    .settings(libraryDependencies++=Seq(Cats, Shapeless))

gitHeadCommitSha in ThisBuild:= git.gitHeadCommit.value
//  {
//  try { Some(Process("git rev-parse HEAD").lines.head) }
//  catch  { case _: Exception =>  None }
//}

lazy val makeVersionProperties = settingKey[Seq[File]]("Makes a version properties file.")

forcegc:= true
publishTo := Some(Resolver.file("root",  new File(Path.userHome.absolutePath+"/.m2/repository")))
publishArtifact := false
//packagedArtifacts in file(".") := Map.empty