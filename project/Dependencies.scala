import sbt._

object Dependencies {
  val ScalaTest       = "org.scalatest"     %% "scalatest"                            % Versions.ScalaTest
  val ScalaCheck      = "org.scalacheck"    %% "scalacheck"                           % Versions.ScalaCheck
  val TypeSafeConfig  = "com.typesafe"      %  "config"                               % Versions.TypeSafeConfig
  val AkkaActor       = "com.typesafe.akka" %% "akka-actor"                           % Versions.AkkaV
  val AkkaStream      = "com.typesafe.akka" %% "akka-stream-experimental"             % Versions.AkkaStreamV
  val AkkaHttpCore    = "com.typesafe.akka" %% "akka-http-core-experimental"          % Versions.AkkaStreamV
  val AkkaHttp        = "com.typesafe.akka" %% "akka-http-experimental"               % Versions.AkkaStreamV
  val SprayJson       = "com.typesafe.akka" %% "akka-http-spray-json-experimental"    % Versions.AkkaStreamV
  val AkkaTestKit     = "com.typesafe.akka" %% "akka-http-testkit-experimental"       % Versions.AkkaStreamV
}