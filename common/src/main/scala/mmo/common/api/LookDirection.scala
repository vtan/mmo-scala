package mmo.common.api

import mmo.common.linear.V2

final case class LookDirection(spriteIndex: Int) extends AnyVal

object LookDirection {
  val down = LookDirection(0)
  val up = LookDirection(3)
  val right = LookDirection(6)
  val left = LookDirection(9)

  def fromVector(v: V2[Double]): LookDirection =
    if (v.x > v.y) {
      if (v.x > -v.y) {
        right
      } else {
        up
      }
    } else {
      if (v.x > -v.y) {
        down
      } else {
        left
      }
    }
}


