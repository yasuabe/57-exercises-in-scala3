val scala3Version = "3.1.2"

lazy val root = project
  .in(file("."))
  .settings(
    name := "57-exercises-scala3",
    version := "0.1.0-SNAPSHOT",

    scalaVersion := scala3Version,

    libraryDependencies ++= List(
      "org.typelevel"     %% "cats-core"        % "2.6.1",
      "org.typelevel"     %% "cats-effect"      % "3.3.11",
      "org.typelevel"     %% "cats-free"        % "2.7.0",
      "eu.timepit"        %% "refined"          % "0.9.29",
      "org.scalameta"     %% "munit"            % "0.7.29" % Test,
      "org.scalacheck"    %% "scalacheck"       % "1.16.0" % Test,
      "org.scalameta"     %% "munit-scalacheck" % "0.7.29" % Test,
      "io.chrisdavenport" %% "cats-scalacheck"  % "0.3.1" % Test
    )
  )