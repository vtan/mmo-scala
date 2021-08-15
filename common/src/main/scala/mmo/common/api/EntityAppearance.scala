package mmo.common.api

import mmo.common.linear.{Rect, V2}

final case class EntityAppearance(
  spriteOffset: Int,
  spriteBoundary: Rect[Double],
  collisionBox: Rect[Double]
) {
  val spriteCenter: V2[Double] = spriteBoundary.center
  val collisionCenter: V2[Double] = collisionBox.center
}
