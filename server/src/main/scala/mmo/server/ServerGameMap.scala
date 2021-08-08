package mmo.server

import mmo.common.api.{CompactGameMap, Id, TileIndex}
import mmo.common.linear.{Rect, V2}
import mmo.common.map.GameMap
import mmo.server.tiled.{TiledMap, TiledObject, Tileset}

final case class ServerGameMap(
  id: Id[ServerGameMap],
  // TODO: store only the obstacle array, not the tile indices
  gameMap: GameMap,
  compactGameMap: CompactGameMap,
  teleports: Seq[ServerGameMap.Teleport]
)

object ServerGameMap {

  final case class Teleport(
    rect: Rect[Double],
    targetPosition: V2[Double],
    targetMapName: String
  )

  def fromTiled(id: Id[ServerGameMap], tiledMap: TiledMap, tileset: Tileset): ServerGameMap = {
    val size = tiledMap.width * tiledMap.height

    val obstaclePositions: Array[Boolean] = Array.fill(size)(false)
    val layersBuilder = Array.newBuilder[Array[TileIndex]]
    val teleportsBuilder = Seq.newBuilder[ServerGameMap.Teleport]

    tiledMap.layers.foreach { layer =>
      layer.data.foreach { data =>
        val tileIndices = Array.fill(size)(TileIndex.empty)

        data.map(tiledMap.resolveGlobalTileId).zipWithIndex.foreach {
          case (Some((localTileId, _)), position) =>
            tileIndices(position) = TileIndex(localTileId.asInt)
            if (tileset.tileById.get(localTileId).exists(_.isObstacle)) {
              obstaclePositions(position) = true
            }
          case (None, _) => ()
        }
        layersBuilder += tileIndices
      }

      layer.objects.foreach(_.foreach {
        case TiledObject.Teleport(pixelRect, targetPosition, targetMapName) =>
          val rect = pixelRect.map(_.toDouble / tiledMap.tilewidth.toDouble)
          teleportsBuilder += ServerGameMap.Teleport(rect, targetPosition.map(_.toDouble), targetMapName)

        case _ => ()
      })
    }

    val frontTileIndexSet = tileset.tileById.filter(_._2.isFront).map(_._1.asInt).toSet

    val gameMap = GameMap(
      width = tiledMap.width,
      height = tiledMap.height,
      layers = layersBuilder.result(),
      obstaclePositions = obstaclePositions,
      frontTileIndices = Array.tabulate(tileset.tilecount)(frontTileIndexSet.contains)
    )

    ServerGameMap(
      id = id,
      gameMap = gameMap,
      compactGameMap = CompactGameMap.from(gameMap),
      teleports = teleportsBuilder.result()
    )
  }
}
