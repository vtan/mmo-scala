package mmo.server.tiled

import io.circe.Decoder
import io.circe.generic.semiauto.deriveDecoder

final case class Tileset(
  tiles: Seq[Tile],
  tilecount: Int
) {
  val tileById: Map[LocalTileId, Tile] = tiles.map(x => x.id -> x).toMap
}

final case class Tile(
  id: LocalTileId,
  `type`: String
) {
  val isObstacle: Boolean = `type`.toLowerCase == "obstacle"
  val isFront: Boolean = `type`.toLowerCase == "front"
}

final case class LocalTileId(asInt: Int) extends AnyVal

object Tileset {
  implicit val localTileIdDecoder: Decoder[LocalTileId] = Decoder.decodeInt.map(LocalTileId)
  implicit val tileDecoder: Decoder[Tile] = deriveDecoder
  implicit val tilesetDecoder: Decoder[Tileset] = deriveDecoder
}
