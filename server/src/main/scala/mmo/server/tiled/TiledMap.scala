package mmo.server.tiled

import mmo.common.api.TileIndex
import mmo.common.map.GameMap

import io.circe.Decoder
import io.circe.generic.semiauto.deriveDecoder
import scala.collection.SortedMap

final case class TiledMap(
  width: Int,
  height: Int,
  tilesets: Seq[TilesetRef],
  layers: Seq[Layer]
) {

  private val tilesetByFirstId: SortedMap[GlobalTileId, TilesetRef] = {
    implicit val reverseGlobalTileIdOrdering: Ordering[GlobalTileId] = Ordering.by[GlobalTileId, Int](_.asInt).reverse

    SortedMap.from(tilesets.map(x => x.firstgid -> x))
  }

  def resolveGlobalTileId(id: GlobalTileId): Option[(LocalTileId, TilesetRef)] =
    tilesetByFirstId.minAfter(id).map {
      case (_, tileset) =>
        val localId = LocalTileId(id.asInt - tileset.firstgid.asInt)
        localId -> tileset
    }

  def toGameMap(tileset: Tileset): GameMap = {
    // TODO don't convert to tile indices and then back below
    val layers = this.layers.map { layer =>
      layer.data.map(resolveGlobalTileId).map {
        case Some((localTileId, _)) => TileIndex(localTileId.asInt)
        case None => TileIndex.empty
      }.toArray
    }.toArray

    val obstaclePositions = (0 until width * height).map { position =>
      layers.exists { layer =>
        val localTileId = LocalTileId(layer(position).asInt)
        tileset.tileById.get(localTileId).exists(_.isObstacle)
      }
    }.toArray

    val frontTileIndexSet = tileset.tileById.filter(_._2.isFront).map(_._1.asInt).toSet

    GameMap(
      width = width,
      height = height,
      layers = layers,
      obstaclePositions = obstaclePositions,
      frontTileIndices = Array.tabulate(tileset.tilecount)(frontTileIndexSet.contains)
    )
  }
}

final case class TilesetRef(
  firstgid: GlobalTileId,
  source: String
)

final case class Layer(
  data: Seq[GlobalTileId]
)

final case class GlobalTileId(asInt: Int) extends AnyVal {
  def isEmpty: Boolean = asInt == 0
}

object TiledMap {
  implicit val tilesetRefDecoder: Decoder[TilesetRef] = deriveDecoder
  implicit val layerDecoder: Decoder[Layer] = deriveDecoder
  implicit val globalTileIdDecoder: Decoder[GlobalTileId] = Decoder.decodeInt.map(GlobalTileId)
  implicit val tiledMapDecoder: Decoder[TiledMap] = deriveDecoder
}

