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
  )
)

lazy val common = (project in file("common")).settings(
  commonSettings,
  libraryDependencies ++= Seq(
    "com.sksamuel.avro4s" %% "avro4s-core" % "4.0.10"
  )
)

lazy val server = (project in file("server")).dependsOn(common).settings {
  val akkaVersion = "2.6.15"
  val circeVersion = "0.14.1"
  commonSettings ++ Seq(
    libraryDependencies ++= Vector(
      "com.typesafe.akka" %% "akka-actor-typed" % akkaVersion,
      "com.typesafe.akka" %% "akka-stream-typed" % akkaVersion,
      "io.circe" %% "circe-core" % circeVersion,
      "io.circe" %% "circe-generic" % circeVersion,
      "io.circe" %% "circe-parser" % circeVersion,
      "ch.qos.logback" % "logback-classic" % "1.2.3"
    ),
    mainClass := Some("mmo.server.Main"),
    run / fork := true,
    assembly / assemblyMergeStrategy := {
      case "module-info.class" => MergeStrategy.discard
      case x =>
        val oldStrategy = (assembly / assemblyMergeStrategy).value
        oldStrategy(x)
    }
  )
}

lazy val client = (project in file("client")).dependsOn(common).settings {
  val lwjglVersion = "3.2.3"

  val arch = {
    val os = sys.props("os.name").toLowerCase
    if (os.contains("linux")) {
      "linux"
    } else if (os.contains("mac")) {
      "macos"
    } else if (os.contains("windows")) {
      "windows"
    } else {
      throw new Exception(s"Cannot determine platform for OS: $os")
    }
  }
  val natives = "natives-" + arch

  commonSettings ++ Seq(
    libraryDependencies ++= Vector(
      "org.lwjgl" % "lwjgl" % lwjglVersion,
      "org.lwjgl" % "lwjgl" % lwjglVersion classifier natives,
      "org.lwjgl" % "lwjgl-glfw" % lwjglVersion,
      "org.lwjgl" % "lwjgl-glfw" % lwjglVersion classifier natives,
      "org.lwjgl" % "lwjgl-nanovg" % lwjglVersion,
      "org.lwjgl" % "lwjgl-nanovg" % lwjglVersion classifier natives,
      "org.lwjgl" % "lwjgl-opengl" % lwjglVersion,
      "org.lwjgl" % "lwjgl-opengl" % lwjglVersion classifier natives
    ),
    mainClass := Some("mmo.client.Main"),

    run / fork := true,
    run / javaOptions ++= (if (arch == "macos") Seq("-XstartOnFirstThread") else Seq())
  )
}