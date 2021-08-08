package mmo.client.game

import mmo.client.graphics.TileAtlas
import mmo.common.linear.{Rect, V2}

final case class WindowGeometry(
  windowSize: V2[Double],
  scaleFactor: Int
) {
  val pixelPerTile: Double = (scaleFactor * TileAtlas.tileSize).toDouble
}

final case class Camera(
  viewport: Rect[Double],
  mapSize: V2[Int],
  windowGeometry: WindowGeometry
) {

  def transformVisibleRect(tile: Rect[Double]): Option[Rect[Double]] =
    if (isRectVisible(tile)) {
      Some(transformRect(tile))
    } else {
      None
    }

  def transformRect(tile: Rect[Double]): Rect[Double] =
    Rect(
      xy = transformPoint(tile.xy),
      wh = windowGeometry.pixelPerTile *: tile.wh
    )

  def isRectVisible(rect: Rect[Double]): Boolean =
    viewport.intersects(rect)

  def transformVisiblePoint(point: V2[Double]): Option[V2[Double]] =
    if (isPointVisible(point)) {
      Some(transformPoint(point))
    } else {
      None
    }

  def transformPoint(point: V2[Double]): V2[Double] =
    (windowGeometry.pixelPerTile *: (point - viewport.xy)).map(_.floor)

  def isPointVisible(point: V2[Double]): Boolean =
    viewport.contains(point)

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

  def centerOn(target: V2[Double], mapSize: V2[Int], windowGeometry: WindowGeometry): Camera = {
    val size = (1.0f / windowGeometry.pixelPerTile) *: windowGeometry.windowSize
    val topLeft = {
      def positionOnAxis(target: Double, windowExtent: Double, mapExtent: Double): Double = {
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
        positionOnAxis(target.x, size.x, mapSize.x.toDouble),
        positionOnAxis(target.y, size.y, mapSize.y.toDouble)
      )
    }
    Camera(
      viewport = Rect(xy = topLeft, wh = size),
      mapSize = mapSize,
      windowGeometry = windowGeometry
    )
  }
}
