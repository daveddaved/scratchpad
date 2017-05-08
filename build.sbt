import ProjectHelper._
import Dependencies._

git.baseVersion := "0.1"

Revolver.settings
//enablePlugins(net.virtualvoid.optimizer.SbtOptimizerPlugin)

name := "Scratch Pad"
scalaVersion in ThisBuild := Versions.ScalaV
scalacOptions in ThisBuild ++= Seq(
  "-Ybackend:GenBCode",
  "-Ydelambdafy:method",
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

lazy val reactiveWeb =makeProject("reactiveWeb",nonStandardDirs=true)
  .dependsOn(common)
  .settings(libraryDependencies++=Seq(
    jdbc,
    cache,
    ws,
    QuillCore,
    QuillCasandra,
    QuillAsync,
    Cassandra.DSCore))
  .enablePlugins(PlayScala, SbtWeb)


lazy val fpinscala = makeProject("fpinscala")
  .dependsOn(common)
  .settings(
    libraryDependencies += "org.typelevel" %% "discipline" % "0.4",
    initialCommands in console := """
        import scratchpad.ch15Streams._
        import SimpleStreamTransducers._
        import Process._"""
  )
//      import mp.applicative._
//      import mp.monads._
//      import Applicative._

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


lazy val `msdemo` = makeProject("msdemo",List("stage","dev","prod"))
                    .settings(libraryDependencies++=Seq(
                      TypeSafeConfig, AkkaActor, AkkaStream, AkkaHttpCore, AkkaHttp, SprayJson,
                      AkkaTestKit % "test,it"
                      )
                    )

lazy val `ms` = makeProject("ms",List("stage","dev","prod"))
  .settings(libraryDependencies++=Seq(
    TypeSafeConfig,
    AkkaActor, AkkaStream, AkkaHttpCore, AkkaHttp, SprayJson,
    QuillCore, QuillCasandra, QuillAsync, Cassandra.DSCore,
    Hikari, // MySqlAsync,
    AkkaTestKit % "test,it"
  ) ++ Slf4j.All
  )


gitHeadCommitSha in ThisBuild:= {
  try { Some(Process("git rev-parse HEAD").lines.head) }
  catch  { case _: Exception =>  None }
}

lazy val makeVersionProperties = settingKey[Seq[File]]("Makes a version properties file.")

forcegc:= true
publishTo := Some(Resolver.file("root",  new File(Path.userHome.absolutePath+"/.m2/repository")))
publishArtifact := false
packagedArtifacts in file(".") := Map.empty