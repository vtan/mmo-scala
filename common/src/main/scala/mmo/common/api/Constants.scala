package mmo.common.api

import mmo.common.linear.{Rect, V2}

object Constants {

  val playerTilePerSecond: Double = 4.0
  val mobTilePerSecond: Double = 0.7

  val playerHitbox: Rect[Double] = Rect(
    xy = V2(0.1, 0.7),
    wh = V2(0.80, 0.3)
  )

  val playerHitboxCenter: V2[Double] = playerHitbox.center

  val playerAttackLength: Double = 0.3

  val playerAttackRangeSq: Double = 1.6 * 1.6

  val playerMaxHitPoints: Int = 20

  val maxMessageBytes: Int = 4096
}
