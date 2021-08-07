package mmo.server

import mmo.server.tiled.{TiledMap, Tileset}

import akka.actor.typed.scaladsl.adapter._
import akka.actor.ActorSystem
import akka.io.Tcp.SO.TcpNoDelay
import akka.stream.scaladsl.{Keep, Source, Tcp}
import akka.stream.scaladsl.Tcp.{IncomingConnection, ServerBinding}
import akka.Done
import java.nio.file.{Files, Path}
import org.slf4j.LoggerFactory
import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success}

object Main {

  def main(args: Array[String]): Unit = {
    val log = LoggerFactory.getLogger(getClass)

    val map = io.circe.parser.decode[TiledMap](
      Files.readString(Path.of("data/map.json"))
    ).toTry.get
    val tileset = io.circe.parser.decode[Tileset](
      Files.readString(Path.of("data/tileset.json"))
    ).toTry.get

    val host = "0.0.0.0"
    val port = 10001

    implicit val system: ActorSystem = ActorSystem(name = "system")
    implicit val ec: ExecutionContext = system.dispatcher

    val gameMap = map.toGameMap(tileset)
    val gameActor = new GameActor(gameMap)
    val gameActorRef = system.spawn(gameActor.start, name = "game")

    val connections: Source[IncomingConnection, Future[ServerBinding]] =
      Tcp().bind(host, port, options = List(TcpNoDelay(on = true)))

    connections.runForeach { connection =>
      val ip = connection.remoteAddress
      log.info(s"$ip connected")
      val flow = SessionFlow.create(gameActorRef).watchTermination()(Keep.right)
      connection.handleWith(flow).onComplete {
        case Success(Done) => log.info(s"$ip disconnected")
        case Failure(ex) => log.warn(s"$ip connection failed", ex)
      }
    }

    log.info(s"Server running on $host:$port")
  }
}
