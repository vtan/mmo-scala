package mmo.common.api

import mmo.common.linear.{Rect, V2}

object Constants {

  val playerTilePerSecond: Float = 4.0f

  val playerHitbox: Rect[Float] = Rect(
    xy = V2(0.05f, 0.7f),
    wh = V2(0.9f, 0.3f)
  )

  val maxMessageBytes: Int = 4096

}
