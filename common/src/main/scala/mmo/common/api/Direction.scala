package mmo.common.api

import mmo.common.linear.V2

final case class Direction(asInt: Int) extends AnyVal {

  def vector: V2[Float] = Direction.vectors(asInt)

  def isMoving: Boolean = asInt != 0
}

object Direction {
  val none = Direction(0)
  val right = Direction(1)
  val down = Direction(2)
  val left = Direction(3)
  val up = Direction(4)

  val vectors: Array[V2[Float]] = Array(
    V2(0, 0),
    V2(1, 0),
    V2(0, 1),
    V2(-1, 0),
    V2(0, -1)
  )
}
