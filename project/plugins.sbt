addSbtPlugin("pl.project13.scala" % "sbt-jmh"        % "0.4.7")
addSbtPlugin("org.scalameta"      % "sbt-scalafmt"   % "2.5.2")
addSbtPlugin("com.github.sbt"     % "sbt-ci-release" % "1.6.1")

addSbtPlugin("org.portable-scala" % "sbt-scalajs-crossproject" % "1.3.2")
addSbtPlugin("org.scala-js"       % "sbt-scalajs"              % "1.16.0")
addSbtPlugin("ch.epfl.scala"      % "sbt-scalajs-bundler"      % "0.21.1")

addSbtPlugin("org.scalameta" % "sbt-mdoc" % "2.5.4")

addSbtPlugin("org.scoverage" % "sbt-scoverage" % "2.2.0")

addSbtPlugin("com.eed3si9n" % "sbt-assembly" % "2.3.1")

libraryDependencies ++= Seq(
    "org.typelevel" %% "scalac-options" % "0.1.7"
)
