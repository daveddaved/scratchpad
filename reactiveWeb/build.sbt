

JsEngineKeys.engineType := JsEngineKeys.EngineType.Node

JsEngineKeys.command := Some(file("/usr/bin/nodejs"))

name := "Reactive Web"

routesGenerator := InjectedRoutesGenerator

com.typesafe.sbt.SbtScalariform.scalariformSettings

libraryDependencies += filters


pipelineStages := Seq(closure)



