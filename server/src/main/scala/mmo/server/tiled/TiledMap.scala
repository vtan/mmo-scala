package mmo.server.tiled

import mmo.common.linear.{Rect, V2}

import io.circe.{Decoder, Json}
import io.circe.generic.semiauto.deriveDecoder
import scala.collection.SortedMap

final case class TiledMap(
  width: Int,
  height: Int,
  tilewidth: Int,
  tileheight: Int,
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
}

final case class TilesetRef(
  firstgid: GlobalTileId,
  source: String
)

final case class Layer(
  data: Option[Seq[GlobalTileId]],
  objects: Option[Seq[TiledObject]]
)

final case class TiledObject(
  `type`: String,
  x: Int,
  y: Int,
  width: Int,
  height: Int,
  properties: Option[Seq[ObjectProperty]]
) {
  def rect: Rect[Int] = Rect(x, y, width, height)
}

final case class ObjectProperty(
  name: String,
  value: Json
)

final case class GlobalTileId(asInt: Int) extends AnyVal {
  def isEmpty: Boolean = asInt == 0
}

object TiledObject {
  object Teleport {
    def unapply(obj: TiledObject): Option[(Rect[Int], V2[Int], String)] =
      if (obj.`type`.toLowerCase == "teleport") {
        val properties = obj.properties.toList.flatten
        for {
          targetMap <- properties.collectFirst {
            case ObjectProperty("targetMap", json) => json.asString
          }.flatten
          targetX <- properties.collectFirst {
            case ObjectProperty("targetX", json) => json.asNumber.flatMap(_.toInt)
          }.flatten
          targetY <- properties.collectFirst {
            case ObjectProperty("targetY", json) => json.asNumber.flatMap(_.toInt)
          }.flatten
        } yield (obj.rect, V2(targetX, targetY), targetMap)
      } else {
        None
      }
  }
}

object TiledMap {
  import io.circe.generic.auto._
  private implicit val globalTileIdDecoder: Decoder[GlobalTileId] = Decoder.decodeInt.map(GlobalTileId)

  implicit val tiledMapDecoder: Decoder[TiledMap] = deriveDecoder
}

