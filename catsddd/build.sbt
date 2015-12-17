import com.trueaccord.scalapb.{ScalaPbPlugin => PB}

PB.protobufSettings

resolvers += Resolver.sonatypeRepo("releases")

addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.8.0")
//addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full)
libraryDependencies +=  "org.scalamacros" %% "resetallattrs" % "1.0.0-M1"