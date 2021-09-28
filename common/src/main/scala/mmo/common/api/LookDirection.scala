package mmo.common.api

import mmo.common.linear.V2

final case class LookDirection(asInt: Int) extends AnyVal

object LookDirection {
  val right = LookDirection(0)
  val down = LookDirection(1)
  val left = LookDirection(2)
  val up = LookDirection(3)

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


