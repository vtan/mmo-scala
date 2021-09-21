package mmo.common.api

import mmo.common.linear.V2

import scala.util.Random

final case class Direction(asInt: Int) extends AnyVal {

  def vector: V2[Double] = Direction.vectors(asInt)
  def lookDirection: LookDirection = Direction.lookDirections(asInt)
  def inverse: Direction = Direction.inverses(asInt)

  def isMoving: Boolean = asInt != 0
}

object Direction {
  val none = Direction(0)
  val right = Direction(1)
  val down = Direction(2)
  val left = Direction(3)
  val up = Direction(4)
  val rightDown = Direction(5)
  val leftDown = Direction(6)
  val leftUp = Direction(7)
  val rightUp = Direction(8)

  def random(implicit rnd: Random): Direction = Direction(1 + rnd.nextInt(8))

  private val vectors: Array[V2[Double]] = Array(
    V2(0, 0),
    V2(1, 0),
    V2(0, 1),
    V2(-1, 0),
    V2(0, -1),
    V2(1.0, 1.0).normalize,
    V2(-1.0, 1.0).normalize,
    V2(-1, -1.0).normalize,
    V2(1.0, -1.0).normalize
  )

  private val lookDirections: Array[LookDirection] = Array(
    // none
    LookDirection.down,
    // movement on one axis
    LookDirection.right,
    LookDirection.down,
    LookDirection.left,
    LookDirection.up,
    // movement on two axes
    LookDirection.right,
    LookDirection.left,
    LookDirection.left,
    LookDirection.right
  )

  private val inverses: Array[Direction] = Array(
    Direction.none,
    Direction.left,
    Direction.up,
    Direction.right,
    Direction.down,
    Direction.leftUp,
    Direction.rightUp,
    Direction.rightDown,
    Direction.leftDown
  )
}
