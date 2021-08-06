package mmo.client.game

import mmo.client.graphics.TileAtlas
import mmo.common.linear.{Rect, V2}

final case class WindowGeometry(
  windowSize: V2[Float],
  scaleFactor: Int
) {
  val pixelPerTile: Float = (scaleFactor * TileAtlas.tileSize).toFloat
}

final case class Camera(
  viewport: Rect[Float],
  mapSize: V2[Int],
  windowGeometry: WindowGeometry
) {

  def transformVisibleRect(tile: Rect[Float]): Option[Rect[Float]] =
    if (isRectVisible(tile)) {
      Some(transformRect(tile))
    } else {
      None
    }

  def transformRect(tile: Rect[Float]): Rect[Float] =
    Rect(
      xy = transformPoint(tile.xy),
      wh = windowGeometry.pixelPerTile *: tile.wh
    )

  def transformPoint(point: V2[Float]): V2[Float] =
    (windowGeometry.pixelPerTile *: (point - viewport.xy)).map(_.floor)

  def isRectVisible(rect: Rect[Float]): Boolean =
    viewport.intersects(rect)

  def visibleTiles: Iterator[V2[Int]] = {
    val V2(minX, minY) = viewport.xy
      .map(n => Math.max(0, n.floor.toInt))
    val V2(maxX, maxY) = viewport.end
      .zipWith(mapSize)((n, mapMax) => Math.min(mapMax, n.ceil.toInt))

    (minX until maxX).iterator.flatMap { x =>
      (minY until maxY).iterator.map { y =>
        V2(x, y)
      }
    }
  }
}

object Camera {

  def centerOn(target: V2[Float], mapSize: V2[Int], windowGeometry: WindowGeometry): Camera = {
    val size = (1.0f / windowGeometry.pixelPerTile) *: windowGeometry.windowSize
    val topLeft = {
      def positionOnAxis(target: Float, windowExtent: Float, mapExtent: Float): Float = {
        val min = target - windowExtent / 2
        val max = target + windowExtent / 2
        val belowMin = min < 0
        val aboveMax = max >= mapExtent
        if (mapExtent < windowExtent) {
          (mapExtent - windowExtent) / 2
        } else if (belowMin) {
          0
        } else if (aboveMax) {
          mapExtent - windowExtent
        } else {
          min
        }
      }

      V2(
        positionOnAxis(target.x, size.x, mapSize.x.toFloat),
        positionOnAxis(target.y, size.y, mapSize.y.toFloat)
      )
    }
    Camera(
      viewport = Rect(xy = topLeft, wh = size),
      mapSize = mapSize,
      windowGeometry = windowGeometry
    )
  }
}
