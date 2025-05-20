import com.github.sbt.git.SbtGit.GitKeys.useConsoleForROGit
import org.scalajs.jsenv.nodejs.*
import org.typelevel.scalacoptions.ScalacOption
import org.typelevel.scalacoptions.ScalacOptions
import org.typelevel.scalacoptions.ScalaVersion

val scala3Version    = "3.7.0"
val kyoVersion       = "0.19.0+4-0155542b+20250516-1656-SNAPSHOT"
val scalaTestVersion = "3.2.19"

val compilerOptions = Set(
    ScalacOptions.encoding("utf8"),
    ScalacOptions.feature,
    ScalacOptions.unchecked,
    ScalacOptions.deprecation,
    ScalacOptions.warnValueDiscard,
    ScalacOptions.languageStrictEquality,
    ScalacOptions.release("11"),
    ScalacOptions.advancedKindProjector
)

ThisBuild / scalaVersion := scala3Version
publish / skip           := true

ThisBuild / organization := "io.getkyo"
ThisBuild / homepage     := Some(url("https://getkyo.io"))
ThisBuild / licenses     := List("Apache-2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0"))
ThisBuild / developers := List(
    Developer(
        "fwbrasil",
        "Flavio Brasil",
        "fwbrasil@gmail.com",
        url("https://github.com/fwbrasil/")
    )
)

ThisBuild / sonatypeCredentialHost := "s01.oss.sonatype.org"
ThisBuild / sonatypeRepository     := "https://s01.oss.sonatype.org/service/local"
ThisBuild / sonatypeProfileName    := "io.getkyo"

ThisBuild / useConsoleForROGit := (baseDirectory.value / ".git").isFile

ThisBuild / assemblyMergeStrategy := { _ => MergeStrategy.first }

lazy val `kyo-ai-settings` = Seq(
    fork               := true,
    scalaVersion       := scala3Version,
    crossScalaVersions := List(scala3Version),
    scalacOptions ++= scalacOptionTokens(compilerOptions).value,
    scalafmtOnCompile := true,
    Test / testOptions += Tests.Argument("-oDG"),
    ThisBuild / versionScheme               := Some("early-semver"),
    libraryDependencies += "org.scalatest" %%% "scalatest" % scalaTestVersion % Test,
    Test / javaOptions += "--add-opens=java.base/java.lang=ALL-UNNAMED"
)

Global / onLoad := {
    val project =
        System.getProperty("platform", "JVM").toUpperCase match {
            case "JVM" => `kyo-aiJVM`
            // case "JS"     => `kyo-aiJS`
            case platform => throw new IllegalArgumentException("Invalid platform: " + platform)
        }

    (Global / onLoad).value andThen { state =>
        "project " + project.id :: state
    }
}

lazy val `kyo-aiJVM` = project
    .in(file("."))
    .settings(
        name := "kyo-aiJVM",
        `kyo-ai-settings`
    )
    .aggregate(
        `kyo-ai`.jvm,
        `kyo-neotypes`.jvm,
        `kyo-container`.jvm,
        `kyo-tastyquery`.jvm,
        `kyo-ai-copilot`.jvm,
        `kyo-ai-mcp`.jvm
    )

// lazy val `kyo-aiJS` = project
//     .in(file("js"))
//     .settings(
//         name := "kyo-aiJS",
//         `kyo-ai-settings`
//     )
//     .aggregate(
//         `kyo-ai`.js,
//         `kyo-ai-arc`.js,
//         `kyo-podman`.js,
//         `kyo-ai-copilot`.js,
//         `kyo-ai-ui`.js
//     )

lazy val `kyo-ai` =
    crossProject(JVMPlatform)
        .withoutSuffixFor(JVMPlatform)
        .crossType(CrossType.Full)
        .in(file("kyo-ai"))
        .settings(
            `kyo-ai-settings`,
            libraryDependencies += "io.getkyo"                    %%% "kyo-actor"       % kyoVersion,
            libraryDependencies += "io.getkyo"                    %%% "kyo-sttp"        % kyoVersion,
            libraryDependencies += "io.getkyo"                    %%% "kyo-playwright"  % kyoVersion,
            libraryDependencies += "dev.zio"                       %% "zio-schema"      % "1.6.3",
            libraryDependencies += "dev.zio"                       %% "zio-schema-json" % "1.6.3",
            libraryDependencies += "com.softwaremill.sttp.client3" %% "zio-json"        % "3.10.3"
        )
// .jsSettings(`js-settings`)

lazy val `kyo-tastyquery` =
    crossProject(JVMPlatform)
        .withoutSuffixFor(JVMPlatform)
        .crossType(CrossType.Full)
        .in(file("kyo-tastyquery"))
        .settings(
            `kyo-ai-settings`,
            libraryDependencies += "ch.epfl.scala" %% "tasty-query" % "1.5.0",
            libraryDependencies += "io.getkyo"    %%% "kyo-core"    % kyoVersion
        )

lazy val `kyo-neotypes` =
    crossProject(JVMPlatform)
        .withoutSuffixFor(JVMPlatform)
        .crossType(CrossType.Full)
        .dependsOn(`kyo-container` % Test)
        .in(file("kyo-neotypes"))
        .settings(
            `kyo-ai-settings`,
            libraryDependencies += "io.github.neotypes" %% "neotypes-core"    % "1.2.1",
            libraryDependencies += "io.github.neotypes" %% "neotypes-generic" % "1.2.1",
            libraryDependencies += "org.neo4j"           % "neo4j"            % "2025.04.0",
            libraryDependencies += "io.getkyo"         %%% "kyo-core"         % kyoVersion
        )

lazy val `kyo-container` =
    crossProject(JVMPlatform)
        .withoutSuffixFor(JVMPlatform)
        .crossType(CrossType.Full)
        .in(file("kyo-container"))
        .settings(
            `kyo-ai-settings`,
            libraryDependencies += "io.getkyo" %%% "kyo-core" % kyoVersion
        )

lazy val `kyo-ai-mcp` =
    crossProject(JVMPlatform)
        .withoutSuffixFor(JVMPlatform)
        .crossType(CrossType.Full)
        .dependsOn(`kyo-ai`)
        .in(file("kyo-ai-mcp"))
        .settings(
            `kyo-ai-settings`,
            libraryDependencies += "io.modelcontextprotocol.sdk" % "mcp"             % "0.10.0",
            libraryDependencies += "ch.qos.logback"              % "logback-classic" % "1.5.18",
            libraryDependencies += "org.scalatest"             %%% "scalatest"       % scalaTestVersion % Test
        )

lazy val `kyo-ai-copilot` =
    crossProject(JVMPlatform)
        .withoutSuffixFor(JVMPlatform)
        .crossType(CrossType.Full)
        .dependsOn(`kyo-ai`)
        .dependsOn(`kyo-container`)
        .in(file("kyo-ai-copilot"))
        .settings(
            `kyo-ai-settings`,
            libraryDependencies += "org.scalatest" %%% "scalatest" % scalaTestVersion % Test
        )
// .jsSettings(`js-settings`)

lazy val `js-settings` = Seq(
    Compile / doc / sources                     := Seq.empty,
    fork                                        := false,
    jsEnv                                       := new NodeJSEnv(NodeJSEnv.Config().withArgs(List("--max_old_space_size=5120"))),
    libraryDependencies += "io.github.cquiroz" %%% "scala-java-time" % "2.5.0" % "provided"
)

def scalacOptionToken(proposedScalacOption: ScalacOption) =
    scalacOptionTokens(Set(proposedScalacOption))

def scalacOptionTokens(proposedScalacOptions: Set[ScalacOption]) = Def.setting {
    val version = ScalaVersion.fromString(scalaVersion.value).right.get
    ScalacOptions.tokensForVersion(version, proposedScalacOptions)
}
