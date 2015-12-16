import ProjectHelper._
import Dependencies._

name := "Scratch Pad"
scalaVersion in ThisBuild := "2.11.7"
scalacOptions in ThisBuild ++= Seq(
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
 "Typesafe Snapshots" at "https://repo.typesafe.com/typesafe/snapshots/")

lazy val reactiveWeb =makeProject("reactiveWeb",nonStandardDirs=true)
  .dependsOn(common)
  .settings()
  .enablePlugins(PlayScala, SbtWeb)

lazy val fpinscala = makeProject("fpinscala")
  .dependsOn(common)
  .settings()

lazy val sbtinaction = makeProject("sbtinaction",List("dev","prod","stage"))
  .dependsOn(common)
  .settings(
    libraryDependencies += Dependencies.TypeSafeConfig
  )

lazy val testprj = makeProject("testprj",List("dev","prod","stage"))
  .dependsOn(common)
  .settings(
    libraryDependencies += Dependencies.TypeSafeConfig
  )

lazy val common = (project in file("common")).settings(
  makeVersionProperties := {
    val propFile = new File((resourceManaged in Compile).value, "version.properties")
    val content = "version=%s" format gitHeadCommitSha.value.getOrElse("SNAPSHOT")
    IO.write(propFile, content)
    Seq(propFile)
  }
)

Revolver.settings

lazy val `msdemo` = makeProject("msdemo",List("stage","dev","prod"))
                    .settings(libraryDependencies++=Seq(
                      TypeSafeConfig, AkkaActor, AkkaStream, AkkaHttpCore, AkkaHttp, SprayJson,
                      AkkaTestKit % "test,it"
                      )
                    )


gitHeadCommitSha in ThisBuild:= {
  try { Some(Process("git rev-parse HEAD").lines.head) }
  catch  {case _: Exception =>  None}
}

lazy val makeVersionProperties = settingKey[Seq[File]]("Makes a version properties file.")
