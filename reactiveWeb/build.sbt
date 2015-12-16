
JsEngineKeys.engineType := JsEngineKeys.EngineType.Node

JsEngineKeys.command := Some(file("/usr/bin/nodejs"))

name := "Reactive Web"

routesGenerator := InjectedRoutesGenerator

routesImport += "binders.PathBinders._"

routesImport += "binders.QueryStringBinders._"

com.typesafe.sbt.SbtScalariform.scalariformSettings

libraryDependencies += filters

pipelineStages := Seq(closure)



