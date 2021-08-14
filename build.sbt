ThisBuild / scalaVersion := "2.13.6"

val commonSettings = Seq(
  scalacOptions ++= Seq(
    "-deprecation",                      // Emit warning and location for usages of deprecated APIs.
    "-encoding", "utf-8",                // Specify character encoding used by source files.
    "-explaintypes",                     // Explain type errors in more detail.
    "-feature",                          // Emit warning and location for usages of features that should be imported explicitly.
    "-unchecked",                        // Enable additional warnings where generated code depends on assumptions.
    "-Xcheckinit",                       // Wrap field accessors to throw an exception on uninitialized access.
    "-Xlint:adapted-args",               // Warn if an argument list is modified to match the receiver.
    "-Xlint:constant",                   // Evaluation of a constant arithmetic expression results in an error.
    "-Xlint:delayedinit-select",         // Selecting member of DelayedInit.
    "-Xlint:doc-detached",               // A Scaladoc comment appears to be detached from its element.
    "-Xlint:inaccessible",               // Warn about inaccessible types in method signatures.
    "-Xlint:infer-any",                  // Warn when a type argument is inferred to be `Any`.
    "-Xlint:missing-interpolator",       // A string literal appears to be missing an interpolator id.
    "-Xlint:nullary-unit",               // Warn when nullary methods return Unit.
    "-Xlint:option-implicit",            // Option.apply used implicit view.
    "-Xlint:package-object-classes",     // Class or object defined in package object.
    "-Xlint:poly-implicit-overload",     // Parameterized overloaded implicit methods are not visible as view bounds.
    "-Xlint:private-shadow",             // A private field (or class parameter) shadows a superclass field.
    "-Xlint:stars-align",                // Pattern sequence wildcard must align with sequence component.
    "-Xlint:type-parameter-shadow",      // A local type parameter shadows a type already in scope.
    "-Xsource:3",                        // Forward source compatibility with Scala 3
    "-Ywarn-dead-code",                  // Warn when dead code is identified.
    "-Ywarn-extra-implicit",             // Warn when more than one implicit parameter section is defined.
    "-Ywarn-numeric-widen",              // Warn when numerics are widened.
    "-Ywarn-unused:implicits",           // Warn if an implicit parameter is unused.
    "-Ywarn-unused:imports",             // Warn if an import selector is not referenced.
    "-Ywarn-unused:locals",              // Warn if a local definition is unused.
    "-Ywarn-unused:params",              // Warn if a value parameter is unused.
    "-Ywarn-unused:patvars",             // Warn if a variable bound in a pattern is unused.
    "-Ywarn-unused:privates",            // Warn if a private member is unused.
    "-Ywarn-value-discard",              // Warn when non-Unit expression results are unused.
    "-Ywarn-macros:after"
  ),
  assembly / assemblyMergeStrategy := {
    case "module-info.class" => MergeStrategy.discard
    case PathList("META-INF", "versions", "9", "module-info.class") => MergeStrategy.discard
    case path =>
      val oldStrategy = (assembly / assemblyMergeStrategy).value
      oldStrategy(path)
  }
)

lazy val common = (project in file("common")).settings(
  commonSettings,
  libraryDependencies ++= Dependencies.common
)

lazy val server = (project in file("server")).dependsOn(common).settings(
  commonSettings ++ Seq(
    libraryDependencies ++= Dependencies.server,
    mainClass := Some("mmo.server.Main"),
    run / fork := true
  )
)

lazy val client = (project in file("client")).dependsOn(common).settings {
  val arch = sys.props.getOrElse("cross.os.name", sys.props("os.name")).toLowerCase match {
    case os if os.contains("linux") => "linux"
    case os if os.contains("mac") => "macos"
    case os if os.contains("windows") => "windows"
    case os => throw new Exception(s"Cannot determine platform for OS: $os")
  }
  commonSettings ++ Seq(
    libraryDependencies ++= Dependencies.client(arch),
    mainClass := Some("mmo.client.Main"),

    run / fork := true,
    run / javaOptions ++= (if (arch == "macos") Seq("-XstartOnFirstThread") else Seq())
  )
}