package mmo.client.graphics

import mmo.common.linear.V2

import org.lwjgl.nanovg.NanoVG.{nvgBeginPath, nvgCreateImage, nvgFill, nvgFillPaint, nvgImagePattern, nvgImageSize, nvgRect, NVG_IMAGE_NEAREST}
import org.lwjgl.nanovg.NVGPaint

final case class TileAtlas(
  imageHandle: Int,
  imageWidth: Int,
  imageHeight: Int
) {

  def render(nvg: Long, screenPosition: V2[Float], tilePosition: V2[Int], tileCount: V2[Int], scaleFactor: Int): Unit = {
    // See https://github.com/memononen/nanovg/issues/348

    val V2(rectX, rectY) = screenPosition
    val V2(tileX, tileY) = tilePosition * tileCount
    val tileW = scaleFactor * TileAtlas.tileSize
    val tileH = scaleFactor * TileAtlas.tileSize
    val patternW = scaleFactor * imageWidth
    val patternH = scaleFactor * imageHeight

    val _ = nvgImagePattern(
      nvg,
      rectX - tileX * tileW,
      rectY - tileY * tileH,
      patternW.toFloat,
      patternH.toFloat,
      0, imageHandle, 1.0f, TileAtlas.paint
    )
    nvgFillPaint(nvg, TileAtlas.paint)
    nvgBeginPath(nvg)
    nvgRect(
      nvg,
      rectX,
      rectY,
      (tileCount.x * tileW).toFloat,
      (tileCount.y * tileH).toFloat
    )
    nvgFill(nvg)
  }
}

object TileAtlas {
  val tileSize: Int = 16

  def loadFromFile(nvg: Long, path: String): TileAtlas = {
    val image = nvgCreateImage(nvg, path, NVG_IMAGE_NEAREST)
    val width: Array[Int] = Array(0)
    val height: Array[Int] = Array(0)
    nvgImageSize(nvg, image, width, height)
    TileAtlas(
      imageHandle = image,
      imageWidth = width(0),
      imageHeight = height(0)
    )
  }

  private val paint = NVGPaint.create()
}
