package mmo.server.game

import mmo.common.api.{Constants, Direction, EntityAppeared, EntityPositionsChanged, Id, LookDirection, MobId}
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
  lastBroadcastTick: Long,
  lastAttackTick: Long
) {

  def collisionBoxCenter: V2[Double] = position + template.appearance.collisionCenter

  def toEvent(dt: Double): EntityAppeared =
    EntityAppeared(
      id = id,
      appearance = template.appearance,
      maxHitPoints = template.maxHitPoints,
      hitPoints = hitPoints,
      position = if (direction.isMoving) {
        position + (dt * Constants.mobTilePerSecond) *: direction.vector
      } else {
        position
      },
      direction = direction,
      lookDirection = lookDirection
    )

  def toPositionChange: EntityPositionsChanged.Entry =
    EntityPositionsChanged.Entry(id, position, direction, lookDirection)
}
