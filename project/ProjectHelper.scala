import sbt._
import sbt.Keys._

object ProjectHelper {

  def addTestConfig(name:String) = config(name) extend IntegrationTest describedAs s"Environment specific settings for $name"

  def standardDirs(root:String):List[File] = {
    val stubs = List("/src/main/scala","/src/main/resources","/src/test/scala", "/src/test/resources")
    stubs.map( s  => file(s"./$root$s"))
  }

  def makeProject(name:String ,environments:Seq[String] = Nil, nonStandardDirs:Boolean=false):Project = {
    val additionalConfigs = environments.map(addTestConfig).toList
    if(!nonStandardDirs) IO.createDirectories(standardDirs(name))
    Project(name, file(name))
        .settings(commonSettings:_*)
        .configs(IntegrationTest)
        .settings(
          version := "1.0-" + gitHeadCommitSha.value.getOrElse("SNAPSHOT"),
          organization := "com.scratchpad",
          libraryDependencies ++= Seq(
          Dependencies.ScalaCheck % "test,it",
          Dependencies.ScalaTest % "test,it"
          )
        ).enablePlugins()
      .settings(Defaults.itSettings:_*)
      .configs(additionalConfigs:_*)
      .settings(additionalConfigs flatMap makeSettingsForConfig:_*)
  }

  lazy val commonSettings = Seq(
    organization := "com.scratchpad",
    version := "0.1.0",
    scalaVersion := Versions.Scala
  )

  lazy val gitHeadCommitSha = settingKey[Option[String]]("Determine the current git commit SHA.")
  def makeSettingsForConfig(config: Configuration): Seq[Setting[_]] ={
    inConfig(config) (Seq(
      fork:= true,
      envVars:= Map("TARGET"->config.name)
    ) ++ Defaults.testTasks
    )
  }
}