package mmo.common.api

import mmo.common.linear.{Rect, V2}

final case class GameMap(
  width: Int,
  height: Int,
  tiles: Array[GameMap.Tile]
) {
  val size: V2[Int] = V2(width, height)

  def tile(x: Int, y: Int): Option[GameMap.Tile] =
    if (x >= 0 && x < width && y >= 0 && y < height) {
      Some(tiles(x + y * width))
    } else {
      None
    }

  def isRectWalkable(hitbox: Rect[Double]): Boolean = {
    val Rect(V2(x, y), V2(w, h)) = hitbox
    val x1 = x.floor.toInt
    val y1 = y.floor.toInt
    val x2 = (x + w).floor.toInt
    val y2 = (y + h).floor.toInt
    val fitsInX = x1 == x2
    val fitsInY = y1 == y2
    tile(x1, y1).exists(_.isWalkable) &&
      (fitsInX || tile(x2, y1).exists(_.isWalkable)) &&
      (fitsInY || tile(x1, y2).exists(_.isWalkable)) &&
      (fitsInX || fitsInY || tile(x2, y2).exists(_.isWalkable))
  }
}

object GameMap {
  final case class Tile(asByte: Byte) {
    def tileIndex: Int = asByte & 0x7f
    def isWalkable: Boolean = (asByte & 0x80) != 0
  }
  object Tile {
    def apply(tileIndex: Int, isWalkable: Boolean): Tile = {
      if (tileIndex >= 128) {
        throw new RuntimeException("Too high tile index")
      } else {
        Tile(((tileIndex & 0x7f) | (if (isWalkable) 1 << 7 else 0)).toByte)
      }
    }
  }
}
