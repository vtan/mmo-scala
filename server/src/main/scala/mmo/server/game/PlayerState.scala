package mmo.server.game

import mmo.common.api.{Direction, EntityAppearance, EntityAppeared, EntityPositionsChanged, Id, LookDirection, PlayerEvent, PlayerId, PlayerStats}
import mmo.common.linear.V2

import akka.stream.scaladsl.SourceQueueWithComplete

final case class PlayerState(
  id: PlayerId,
  name: String,
  mapId: Id[ServerGameMap],
  position: V2[Double],
  direction: Direction,
  lookDirection: LookDirection,
  hitPoints: Int,
  maxHitPoints: Int,
  stats: PlayerStats,
  queue: SourceQueueWithComplete[PlayerEvent],
  receivedAtNano: ServerTime,
  attackStartedAt: ServerTime,
  appearance: EntityAppearance
) {

  def collisionBoxCenter: V2[Double] = position + appearance.collisionCenter

  def isAlive: Boolean = hitPoints > 0

  def toAppearEvent: EntityAppeared =
    EntityAppeared(
      id = id,
      appearance = appearance,
      maxHitPoints = maxHitPoints,
      hitPoints = hitPoints,
      position = position,
      direction = direction,
      lookDirection = lookDirection,
      speed = ServerConstants.playerSpeed
    )

  def toPositionChange: EntityPositionsChanged.Entry =
    EntityPositionsChanged.Entry(
      entityId = id,
      position = position,
      direction = direction,
      lookDirection = lookDirection,
      speed = ServerConstants.playerSpeed
    )
}
