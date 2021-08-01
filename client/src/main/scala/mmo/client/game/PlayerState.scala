package mmo.client.game

import mmo.common.linear.V2

final case class PlayerState(
  position: V2[Float],
  previousPosition: V2[Float],
  lastPositionFromServer: V2[Float]
) {

  val directionIndex: Int = {
    val (down, up, right, left) = (0, 1, 2, 3)
    val V2(dx, dy) = position - previousPosition
    if (dx < 0) {
      left
    } else if (dx > 0) {
      right
    } else if (dy < 0) {
      up
    } else {
      down
    }
  }
}
