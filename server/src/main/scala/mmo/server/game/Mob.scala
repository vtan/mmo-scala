package mmo.server.game

import mmo.common.api.{Direction, EntityAppeared, EntityPositionsChanged, Id, MobId, PlayerId}
import mmo.common.linear.V2
import mmo.server.game.ServerGameMap.MobSpawn

final case class Mob(
  id: MobId,
  template: MobTemplate,
  spawn: MobSpawn,
  mapId: Id[ServerGameMap],
  position: V2[Double],
  direction: Direction,
  nextPosition: V2[Double],
  hitPoints: Int,
  lastBroadcastTick: Tick,
  attackTarget: Option[PlayerId],
  lastAttackTick: Tick
) {

  lazy val speed: Double =
    if (attackTarget.isDefined) template.aggroSpeed else template.idleSpeed

  def collisionBoxCenter: V2[Double] = position + template.appearance.collisionCenter

  def toEvent(fractionOfTick: Double): EntityAppeared =
    EntityAppeared(
      id = id,
      appearance = template.appearance,
      maxHitPoints = template.maxHitPoints,
      hitPoints = hitPoints,
      position = if (direction.isMoving) {
        V2.lerp(fractionOfTick, position, nextPosition)
      } else {
        position
      },
      direction = direction,
      lookDirection = direction.lookDirection,
      speed = speed
    )

  def toPositionChange: EntityPositionsChanged.Entry =
    EntityPositionsChanged.Entry(id, position, direction, direction.lookDirection, speed)
}
