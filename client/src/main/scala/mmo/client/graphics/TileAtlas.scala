package mmo.client.graphics

import mmo.common.linear.{Rect, V2}

import org.lwjgl.nanovg.NanoVG._
import org.lwjgl.nanovg.NVGPaint

final case class TileAtlas(
  imageHandle: Int,
  imageWidth: Int,
  imageHeight: Int
) {

  def render(nvg: Long, rectOnScreen: Rect[Float], positionOnTexture: V2[Int], scaleFactor: Int): Unit = {
    // See https://github.com/memononen/nanovg/issues/348

    val textureOffset = ((scaleFactor * TileAtlas.tileSize) *: positionOnTexture).map(_.toFloat)
    val patternPosition = rectOnScreen.xy - textureOffset
    val _ = nvgImagePattern(
      nvg,
      patternPosition.x,
      patternPosition.y,
      (scaleFactor * imageWidth).toFloat,
      (scaleFactor * imageHeight).toFloat,
      0, imageHandle, 1.0f, TileAtlas.paint
    )
    nvgFillPaint(nvg, TileAtlas.paint)
    nvgBeginPath(nvg)
    nvgRect(
      nvg,
      rectOnScreen.xy.x,
      rectOnScreen.xy.y,
      rectOnScreen.wh.x,
      rectOnScreen.wh.y
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
