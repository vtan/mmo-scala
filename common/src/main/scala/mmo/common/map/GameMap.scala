package mmo.common.map

import mmo.common.api.TileIndex
import mmo.common.linear.{Rect, V2}

final case class GameMap(
  width: Int,
  height: Int,
  layers: Array[Array[TileIndex]],
  obstaclePositions: Array[Boolean],
  frontTileIndices: Array[Boolean]
) extends Rectangular {

  val size: V2[Int] = V2(width, height)

  def isObstacle(x: Int, y: Int): Boolean =
    offsetOf(x, y).forall { offset =>
      obstaclePositions(offset)
    }

  def doesRectCollide(hitbox: Rect[Double]): Boolean = {
    val Rect(V2(x, y), V2(w, h)) = hitbox
    val x1 = x.floor.toInt
    val y1 = y.floor.toInt
    val x2 = (x + w).floor.toInt
    val y2 = (y + h).floor.toInt
    val fitsInX = x1 == x2
    val fitsInY = y1 == y2
    isObstacle(x1, y1) ||
      (!fitsInX && isObstacle(x2, y1)) ||
      (!fitsInY && isObstacle(x1, y2)) ||
      (!fitsInX && !fitsInY && isObstacle(x2, y2))
  }
}
