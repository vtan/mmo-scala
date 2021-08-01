package mmo.client.game

import mmo.client.common.V2

final case class PlayerState(
  position: V2[Int],
  previousPosition: V2[Int]
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
