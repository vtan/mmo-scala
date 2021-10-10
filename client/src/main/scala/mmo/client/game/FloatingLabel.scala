package mmo.client.game

import mmo.common.linear.V2

import org.lwjgl.nanovg.NVGColor

final case class FloatingLabel(
  initialPosition: V2[Double],
  startTime: Double,
  label: String,
  color: NVGColor
)

object FloatingLabel {
  val tilePerSecond = -0.5
  val duration = 1.0
}
