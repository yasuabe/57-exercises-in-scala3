val scala3Version = "3.1.2"

val http4sVersion = "0.23.12"

lazy val root = project
  .in(file("."))
  .settings(
    name := "57-exercises-scala3",
    version := "0.1.0-SNAPSHOT",

    scalaVersion := scala3Version,
    resolvers += Resolver.sonatypeRepo("snapshots"),
    libraryDependencies ++= List(
      "org.typelevel"     %% "cats-core"           % "2.6.1",
      "org.typelevel"     %% "cats-effect"         % "3.3.11",
      "org.typelevel"     %% "cats-free"           % "2.7.0",
      "eu.timepit"        %% "refined"             % "0.9.29",
      "co.fs2"            %% "fs2-core"            % "3.2.7",
      "co.fs2"            %% "fs2-io"              % "3.2.7",
      "org.http4s"        %% "http4s-dsl"          % http4sVersion,
      "org.http4s"        %% "http4s-circe"        % http4sVersion,
      "org.http4s"        %% "http4s-ember-client" % http4sVersion,
      "org.http4s"        %% "http4s-ember-server" % http4sVersion,
      "io.circe"          %% "circe-core"          % "0.15.0-M1",
      "io.circe"          %% "circe-parser"        % "0.15.0-M1",
      "io.circe"          %% "circe-generic"       % "0.15.0-M1",
      "io.circe"          %% "circe-refined"       % "0.15.0-M1",
      "dev.profunktor"    %% "redis4cats-effects"  % "1.2.0",
      "org.typelevel"     %% "log4cats-slf4j"      % "2.3.1",
      "ch.qos.logback" % "logback-classic" % "1.2.10",
      "org.scalameta"     %% "munit"               % "0.7.29" % Test,
      "org.scalacheck"    %% "scalacheck"          % "1.16.0" % Test,
      "org.scalameta"     %% "munit-scalacheck"    % "0.7.29" % Test,
      "io.chrisdavenport" %% "cats-scalacheck"     % "0.3.1" % Test
    )
  )
