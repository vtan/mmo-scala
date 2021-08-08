package mmo.server

import mmo.common.api.CompactGameMap
import mmo.common.map.GameMap

final case class ServerGameMap(
  // TODO: store only the obstacle array, not the tile indices
  gameMap: GameMap,
  compactGameMap: CompactGameMap
)

object ServerGameMap {

  def from(gameMap: GameMap): ServerGameMap =
    ServerGameMap(
      gameMap = gameMap,
      compactGameMap = CompactGameMap.from(gameMap)
    )
}
