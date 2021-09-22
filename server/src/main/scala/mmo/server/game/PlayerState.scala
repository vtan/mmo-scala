package mmo.server.game

import mmo.common.api.{Constants, Direction, EntityAppearance, EntityAppeared, EntityPositionsChanged, Id, LookDirection, PlayerEvent, PlayerId}
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
  queue: SourceQueueWithComplete[PlayerEvent],
  receivedAtNano: ServerTime,
  attackStartedAt: ServerTime
) {

  def collisionBoxCenter: V2[Double] = position + Constants.playerHitboxCenter

  def toAppearEvent: EntityAppeared =
    EntityAppeared(
      id = id,
      appearance = EntityAppearance.empty,
      maxHitPoints = Constants.playerMaxHitPoints,
      hitPoints = hitPoints,
      position = position,
      direction = direction,
      lookDirection = lookDirection
    )

  def toPositionChange: EntityPositionsChanged.Entry =
    EntityPositionsChanged.Entry(id, position, direction, lookDirection)
}
