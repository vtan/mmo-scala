package mmo.common.api

import mmo.common.linear.{Rect, V2}

import scala.collection.immutable.ArraySeq

final case class EntityAppearance(
  height: Int,
  spriteOffsets: SpriteOffsets,
  spriteBoundary: Rect[Double],
  collisionBox: Rect[Double]
) {
  val spriteCenter: V2[Double] = spriteBoundary.center
  val collisionCenter: V2[Double] = collisionBox.center
}

final case class SpriteOffsets(
  right: Int,
  down: Int,
  left: Int,
  up: Int
) {
  private lazy val movementOffsets: ArraySeq[Int] = ArraySeq(right, down, left, up)

  def movement(lookDirection: LookDirection): Int =
    movementOffsets(lookDirection.asInt)
}
