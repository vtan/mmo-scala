import sbt._

object Dependencies {

  private val akkaVersion = "2.6.15"
  private val circeVersion = "0.14.1"
  private val lwjglVersion = "3.2.3"

  val common = Seq(
    "com.sksamuel.avro4s" %% "avro4s-core" % "4.0.10",
    "ch.qos.logback" % "logback-classic" % "1.2.6"
  )

  val server = common ++ Seq(
    "com.typesafe.akka" %% "akka-actor-typed" % akkaVersion,
    "com.typesafe.akka" %% "akka-stream-typed" % akkaVersion,
    "io.circe" %% "circe-core" % circeVersion,
    "io.circe" %% "circe-generic" % circeVersion,
    "io.circe" %% "circe-parser" % circeVersion
  )

  def client(arch: String) = {
    val natives = "natives-" + arch
    common ++ Seq(
      "org.lwjgl" % "lwjgl" % lwjglVersion,
      "org.lwjgl" % "lwjgl" % lwjglVersion classifier natives,
      "org.lwjgl" % "lwjgl-glfw" % lwjglVersion,
      "org.lwjgl" % "lwjgl-glfw" % lwjglVersion classifier natives,
      "org.lwjgl" % "lwjgl-nanovg" % lwjglVersion,
      "org.lwjgl" % "lwjgl-nanovg" % lwjglVersion classifier natives,
      "org.lwjgl" % "lwjgl-opengl" % lwjglVersion,
      "org.lwjgl" % "lwjgl-opengl" % lwjglVersion classifier natives
    )
  }
}
