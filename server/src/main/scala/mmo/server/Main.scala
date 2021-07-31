package mmo.server

import akka.actor.typed.scaladsl.adapter._
import akka.actor.ActorSystem
import akka.stream.scaladsl.{Source, Tcp}
import akka.stream.scaladsl.Tcp.{IncomingConnection, ServerBinding}
import scala.concurrent.Future

object Main {

  def main(args: Array[String]): Unit = {
    val host = "0.0.0.0"
    val port = 10001

    implicit val system: ActorSystem = ActorSystem(name = "system")
    val gameActor = system.spawn(GameActor.start, name = "game")

    val connections: Source[IncomingConnection, Future[ServerBinding]] =
      Tcp().bind(host, port)

    connections.runForeach { connection =>
      println(s"New connection from: ${connection.remoteAddress}")

      val flow = SessionFlow.create(gameActor)
      connection.handleWith(flow)
    }
  }
}
