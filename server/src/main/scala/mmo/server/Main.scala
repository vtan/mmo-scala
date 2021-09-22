package mmo.server

import mmo.common.api.{CompactGameMap, EntityAppearance, Id}
import mmo.common.linear.Rect
import mmo.server.game.{MobTemplate, ServerGameMap}
import mmo.server.server.{GameActor, SessionFlow}
import mmo.server.tiled.{TiledMap, Tileset}

import akka.actor.typed.scaladsl.adapter._
import akka.actor.ActorSystem
import akka.io.Tcp.SO.TcpNoDelay
import akka.stream.scaladsl.{Keep, Source, Tcp}
import akka.stream.scaladsl.Tcp.{IncomingConnection, ServerBinding}
import com.sksamuel.avro4s.AvroOutputStream
import java.io.ByteArrayOutputStream
import java.nio.file.{Files, Path}
import org.slf4j.LoggerFactory
import scala.concurrent.{ExecutionContext, Future}

object Main {

  def main(args: Array[String]): Unit = {
    val log = LoggerFactory.getLogger(getClass)
    log.info("Starting server")

    val tileset = io.circe.parser.decode[Tileset](
      Files.readString(Path.of("data/tileset.json"))
    ).toTry.get

    val (mapEntries, mapNameEntries) = Seq("map", "map2").zipWithIndex.map {
      case (name, index) =>
        val path = Path.of(s"data/$name.json")
        val tiledMap = io.circe.parser.decode[TiledMap](Files.readString(path)).toTry.get
        val id = Id[ServerGameMap](index.toLong)
        val map = ServerGameMap.fromTiled(id, tiledMap, tileset)
        log.info(s"Map '$name' is ${calculateMapLength(map.compactGameMap)} bytes")
        (id -> map, name -> id)
    }.unzip
    val maps = mapEntries.toMap
    val mapNames = mapNameEntries.toMap
    // TODO: read from json
    val mobTemplates = Seq(
      MobTemplate(
        name = "green-slime",
        appearance = EntityAppearance(
          spriteOffset = 64,
          spriteBoundary = Rect(0, 3.0 / 16, 1, 1 - 3.0 / 16),
          collisionBox = Rect(0.02, 0.75, 1 - 0.04, 0.25)
        ),
        maxHitPoints = 6,
        attackCooldownTicks = 10
      ),
      MobTemplate(
        name = "blue-slime",
        appearance = EntityAppearance(
          spriteOffset = 65,
          spriteBoundary = Rect(0, 3.0 / 16, 1, 1 - 3.0 / 16),
          collisionBox = Rect(0.02, 0.75, 1 - 0.04, 0.25)
        ),
        maxHitPoints = 10,
        attackCooldownTicks = 8
      )
    ).map(t => t.name -> t).toMap

    val host = "0.0.0.0"
    val port = 10001

    implicit val system: ActorSystem = ActorSystem(name = "system")
    implicit val ec: ExecutionContext = system.dispatcher

    val gameActor = new GameActor(maps, mapNames, mobTemplates)
    val gameActorRef = system.spawn(gameActor.start, name = "game")

    val connections: Source[IncomingConnection, Future[ServerBinding]] =
      Tcp().bind(host, port, options = List(TcpNoDelay(on = true)))

    connections.runForeach { connection =>
      val flow = SessionFlow.create(gameActorRef).watchTermination()(Keep.right)
      connection.handleWith(flow).onComplete { _ =>
        // TODO: add metrics
      }
    }

    log.info(s"Server running on $host:$port")
  }

  private def calculateMapLength(compactGameMap: CompactGameMap): Int = {
    val baos = new ByteArrayOutputStream()
    val aos = AvroOutputStream.binary[CompactGameMap].to(baos).build()
    aos.write(compactGameMap)
    aos.close()
    baos.size()
  }
}
