package mmo.client.graphics

import mmo.common.linear.V2

import org.lwjgl.nanovg.NanoVG._
import org.lwjgl.nanovg.NVGColor

class ScreenFader(
  nvg: Long
) {

  private var startAt: Double = Double.NaN
  private var endAt: Double = Double.NaN
  private val color: NVGColor = GlfwUtil.color(0, 0, 0)

  def startFadeIn(now: Double, length: Double): Unit = {
    startAt = now
    endAt = now + length
  }

  def isFading(now: Double): Boolean =
    now >= startAt && now < endAt

  def render(now: Double, windowSize: V2[Double]): Unit =
    if (isFading(now)) {
      val t = (now - startAt) / (endAt - startAt)
      val alpha = (1.0 - t) * (1.0 - t)
      color.a(alpha.toFloat)

      nvgFillColor(nvg, color)
      nvgBeginPath(nvg)
      nvgRect(nvg, 0, 0, windowSize.x.toFloat, windowSize.y.toFloat)
      nvgFill(nvg)
    }
}
