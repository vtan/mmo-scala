package mmo.server.game

import mmo.common.api.{Direction, EntityPositionsChanged, Id, LookDirection, MobAppeared, MobId}
import mmo.common.linear.V2
import mmo.server.game.ServerGameMap.MobSpawn

final case class Mob(
  id: MobId,
  template: MobTemplate,
  spawn: MobSpawn,
  mapId: Id[ServerGameMap],
  position: V2[Double],
  direction: Direction,
  lookDirection: LookDirection,
  hitPoints: Int,
  lastBroadcastTick: Long
) {

  def toEvent: MobAppeared =
    MobAppeared(
      id = id,
      appearance = template.appearance,
      maxHitPoints = template.maxHitPoints,
      hitPoints = hitPoints,
      position = position,
      direction = direction,
      lookDirection = lookDirection
    )

  def toPositionChange: EntityPositionsChanged.Entry =
    EntityPositionsChanged.Entry(id, position, direction, lookDirection)
}
