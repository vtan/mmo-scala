package mmo.client.game

import mmo.client.graphics.{GlfwUtil, TileAtlas}
import mmo.common.linear.V2

import org.lwjgl.nanovg.NanoVG._
import org.lwjgl.nanovg.NanoVGGL3._

class Resources(
  window: Long
) {
  val nvg: Long = nvgCreate(0)
  val (windowSize: V2[Double], pixelRatio: Double) = GlfwUtil.getWindowGeometry(window)
  val windowGeometry: WindowGeometry = WindowGeometry(
    windowSize = windowSize,
    scaleFactor = 3
  )

  val charAtlas: TileAtlas = TileAtlas.loadFromFile(nvg, "assets/charset.png")
  val tileAtlas: TileAtlas = TileAtlas.loadFromFile(nvg, "assets/tileset.png")
  val font: Int = nvgCreateFont(nvg, "Roboto", "assets/roboto/Roboto-Regular.ttf")
  nvgFontFaceId(nvg, font)

  def nvgBeginFrame(): Unit =
    org.lwjgl.nanovg.NanoVG.nvgBeginFrame(nvg, windowSize.x.toFloat, windowSize.y.toFloat, pixelRatio.toFloat)
}
