package mmo.client.game

import mmo.common.api.{Constants, Direction, LookDirection, PlayerPositionsChanged}
import mmo.common.linear.V2

final case class EntityState(
  position: V2[Double],
  lastPositionFromServer: V2[Double],
  interpolationSource: V2[Double],
  interpolationTarget: V2[Double],
  direction: Direction,
  lookDirection: LookDirection,
  directionLastChangedAt: Double,
  lastServerEventAt: Double
) {

  def applyPositionChange(update: PlayerPositionsChanged.Entry, now: Double, calculateInterpolation: Boolean): EntityState =
    copy(
      position = update.position,
      lastPositionFromServer = update.position,
      direction = update.direction,
      lookDirection = update.lookDirection,
      directionLastChangedAt = if (this.lookDirection == update.lookDirection) this.directionLastChangedAt else now,
      lastServerEventAt = now,

      interpolationSource = if (calculateInterpolation) {
        this.position
      } else {
        V2.zero
      },
      interpolationTarget = if (calculateInterpolation) {
        update.position + (EntityState.interpolationPeriod * Constants.entityTilePerSecond) *: update.direction.vector
      } else {
        V2.zero
      }
    )

  def spriteIndexAt(time: Double): Int =
    if (direction.isMoving) {
      val offset = (((time - directionLastChangedAt) / 0.15f) % 4).toInt match {
        case 0 => 1
        case 1 => 0
        case 2 => 2
        case 3 => 0
      }
      lookDirection.spriteIndex + offset
    } else {
      lookDirection.spriteIndex
    }
}

object EntityState {
  val interpolationPeriod = 0.5

  def newAt(update: PlayerPositionsChanged.Entry, now: Double): EntityState =
    EntityState(
      position = update.position,
      lastPositionFromServer = update.position,
      interpolationSource = update.position,
      interpolationTarget = update.position,
      direction = update.direction,
      lookDirection = update.lookDirection,
      directionLastChangedAt = now,
      lastServerEventAt = now
    )
}
