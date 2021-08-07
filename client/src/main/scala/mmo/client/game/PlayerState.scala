package mmo.client.game

import mmo.common.api.{Direction, LookDirection}
import mmo.common.linear.V2

final case class PlayerState(
  position: V2[Double],
  lastPositionFromServer: V2[Double],
  smoothedPositionAtLastServerUpdate: V2[Double],
  direction: Direction,
  lookDirection: LookDirection,
  receivedAt: Double,
  directionLastChangedAt: Double
) {
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
