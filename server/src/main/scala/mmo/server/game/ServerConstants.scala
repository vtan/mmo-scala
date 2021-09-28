package mmo.server.game

import mmo.common.api.{EntityAppearance, SpriteOffsets}
import mmo.common.linear.{Rect, V2}

object ServerConstants {

  val playerCollisionBox: Rect[Double] = Rect(xy = V2(0.1, 0.7), wh = V2(0.80, 0.3))

  val playerCollisionBoxCenter: V2[Double] = playerCollisionBox.center

  val playerAppearance = EntityAppearance(
    height = 2,
    spriteOffsets = SpriteOffsets(right = 6, down = 0, left = 9, up = 3),
    spriteBoundary = Rect(-1.0, 0.0, 1.0, 2.0),
    collisionBox = playerCollisionBox
  )
}
