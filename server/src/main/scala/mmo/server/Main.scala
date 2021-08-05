package mmo.server

import akka.actor.typed.scaladsl.adapter._
import akka.actor.ActorSystem
import akka.io.Tcp.SO.TcpNoDelay
import akka.stream.scaladsl.{Keep, Source, Tcp}
import akka.stream.scaladsl.Tcp.{IncomingConnection, ServerBinding}
import akka.Done
import org.slf4j.LoggerFactory
import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success}

object Main {

  def main(args: Array[String]): Unit = {
    val log = LoggerFactory.getLogger(getClass)

    val host = "0.0.0.0"
    val port = 10001

    implicit val system: ActorSystem = ActorSystem(name = "system")
    implicit val ec: ExecutionContext = system.dispatcher

    val gameActor = system.spawn(GameActor.start, name = "game")

    val connections: Source[IncomingConnection, Future[ServerBinding]] =
      Tcp().bind(host, port, options = List(TcpNoDelay(on = true)))

    connections.runForeach { connection =>
      val ip = connection.remoteAddress
      log.info(s"$ip connected")
      val flow = SessionFlow.create(gameActor).watchTermination()(Keep.right)
      connection.handleWith(flow).onComplete {
        case Success(Done) => log.info(s"$ip disconnected")
        case Failure(ex) => log.warn(s"$ip connection failed", ex)
      }
    }

    log.info(s"Server running on $host:$port")
  }
}
