package mmo.client.game

import mmo.common.linear.V2

final case class DamageLabel(
  initialPosition: V2[Double],
  startTime: Double,
  label: String
)

object DamageLabel {
  val tilePerSecond = -0.5
  val duration = 1.0
}
