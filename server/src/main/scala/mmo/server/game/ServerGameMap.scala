package mmo.server.game

import mmo.common.api.{CompactGameMap, Id, TileIndex}
import mmo.common.linear.{Rect, V2}
import mmo.common.map.GameMap
import mmo.server.tiled.{TiledMap, TiledObject, Tileset}

final case class ServerGameMap(
  id: Id[ServerGameMap],
  // TODO: store only the obstacle array, not the tile indices
  gameMap: GameMap,
  compactGameMap: CompactGameMap,
  teleports: Seq[ServerGameMap.Teleport],
  mobSpawns: Seq[ServerGameMap.MobSpawn]
)

object ServerGameMap {

  final case class Teleport(
    rect: Rect[Double],
    targetPosition: V2[Double],
    targetMapName: String
  )

  final case class MobSpawn(
    mapId: Id[ServerGameMap],
    position: V2[Double],
    templateName: String
  )

  def fromTiled(id: Id[ServerGameMap], tiledMap: TiledMap, tileset: Tileset): ServerGameMap = {
    val size = tiledMap.width * tiledMap.height

    val obstaclePositions: Array[Boolean] = Array.fill(size)(false)
    val layersBuilder = Array.newBuilder[Array[TileIndex]]
    val teleportsBuilder = Seq.newBuilder[ServerGameMap.Teleport]
    val mobSpawnBuilder = Seq.newBuilder[ServerGameMap.MobSpawn]

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
        case TiledObject.MobSpawn(pixelPosition, templateName) =>
          val position = pixelPosition.xy.map(_.toDouble / tiledMap.tilewidth.toDouble)
          mobSpawnBuilder += ServerGameMap.MobSpawn(id, position, templateName)

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
      teleports = teleportsBuilder.result(),
      mobSpawns = mobSpawnBuilder.result()
    )
  }
}
