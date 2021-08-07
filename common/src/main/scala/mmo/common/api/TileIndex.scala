package mmo.common.api

import mmo.common.linear.V2

final case class TileIndex(asInt: Int) extends AnyVal {

  def isEmpty: Boolean = asInt < 0

  def toTexturePosition: V2[Int] =
    V2(asInt % 16, asInt / 16)
}

object TileIndex {
  val empty = TileIndex(-1)
}
