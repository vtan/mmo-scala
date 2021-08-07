package mmo.common.api

import mmo.common.linear.{Rect, V2}

object Constants {

  val playerTilePerSecond: Double = 4.0

  val playerHitbox: Rect[Double] = Rect(
    xy = V2(0.075, 0.7),
    wh = V2(0.85, 0.3)
  )

  val maxMessageBytes: Int = 4096

}
