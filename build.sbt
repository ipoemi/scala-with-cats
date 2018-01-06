import Dependencies._
import sbt.Keys.libraryDependencies

val catsVersion = "1.0.1"
val catsCore = "org.typelevel" %% "cats-core" % catsVersion

lazy val root = (project in file("."))
  .settings(commonSettings: _*)
  .settings(
    inThisBuild(
      List(
        organization := "ipoemi",
        scalaVersion := "2.12.4",
        version := "0.1.0-SNAPSHOT"
      )),
    licenses += ("MIT", url("http://opensource.org/licenses/MIT")),
    name := "scala-with-cats",
    libraryDependencies ++= List(
      catsCore,
      scalaTest % Test
    )
  )

lazy val commonSettings = List(
  scalacOptions ++= List(
    "-deprecation", // Emit warning and location for usages of deprecated APIs.
    "-encoding",
    "utf-8", // Specify character encoding used by source files.
    //"-explaintypes", // Explain type errors in more detail.
    "-feature", // Emit warning and location for usages of features that should be imported explicitly.
    //"-language:existentials", // Existential types (besides wildcard types) can be written and inferred
    //"-language:higherKinds", // Allow higher-kinded types
    //"-language:implicitConversions", // Allow definition of implicit functions called views
    //"-language:postfixOps", // Allow post fix operations
    "-unchecked", // Enable additional warnings where generated code depends on assumptions.
    //"-Xcheckinit", // Wrap field accessors to throw an exception on uninitialized access.
    "-Xfatal-warnings", // Fail the compilation if there are any warnings.
    "-Xfuture", // Turn on future language features.
    //"-Xlint:adapted-args", // Warn if an argument list is modified to match the receiver.
    //"-Xlint:constant", // Evaluation of a constant arithmetic expression results in an error.
    //"-Xlint:doc-detached", // A Scaladoc comment appears to be detached from its element.
    //"-Xlint:inaccessible", // Warn about inaccessible types in method signatures.
    //"-Xlint:infer-any", // Warn when a type argument is inferred to be `Any`.
    //"-Xlint:nullary-override", // Warn when non-nullary `def f()' overrides nullary `def f'.
    //"-Xlint:nullary-unit", // Warn when nullary methods return Unit.
    //"-Xlint:poly-implicit-overload", // Parameterized overloaded implicit methods are not visible as view bounds.
    //"-Xlint:stars-align", // Pattern sequence wildcard must align with sequence component.
    //"-Xlint:type-parameter-shadow", // A local type parameter shadows a type already in scope.
    //"-Xlint:unsound-match", // Pattern match may not be typesafe.
    //"-Yno-adapted-args", // Do not adapt an argument list (either by inserting () or creating a tuple) to match the receiver.
    //"-Ywarn-dead-code", // Warn when dead code is identified.
    //"-Ywarn-extra-implicit", // Warn when more than one implicit parameter section is defined.
    //"-Ywarn-inaccessible", // Warn about inaccessible types in method signatures.
    //"-Ywarn-infer-any", // Warn when a type argument is inferred to be `Any`.
    //"-Ywarn-nullary-override", // Warn when non-nullary `def f()' overrides nullary `def f'.
    //"-Ywarn-nullary-unit", // Warn when nullary methods return Unit.
    //"-Ywarn-numeric-widen", // Warn when numerics are widened.
    //"-Ywarn-unused:implicits", // Warn if an implicit parameter is unused.
    //"-Ywarn-unused:params", // Warn if a value parameter is unused.
    //"-Ywarn-unused:patvars", // Warn if a variable bound in a pattern is unused.
    //"-Ywarn-value-discard",
    "-Ypartial-unification" // Enable partial unification in type constructor inference
  ),
  javaOptions in run ++= List(
    "-Xms512M",
    "-Xmx2G",
    "-Xss8M",
    "-XX:+UseG1GC"
  ),
  fork in run := true,
  resolvers ++= List(Resolver.sonatypeRepo("snapshots"),
    Resolver.sonatypeRepo("releases"))
)
