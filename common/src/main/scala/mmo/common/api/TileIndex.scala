package mmo.common.api

import mmo.common.linear.V2

final case class TileIndex(asInt: Int) extends AnyVal {

  def isEmpty: Boolean = asInt < 0

  def toOption: Option[Int] =
    if (isEmpty) {
      None
    } else {
      Some(asInt)
    }

  def toTexturePosition: V2[Int] =
    V2(asInt % 16, asInt / 16)
}

object TileIndex {
  val empty: TileIndex = TileIndex(-1)

  def fromOption(int: Option[Int]): TileIndex =
    int.fold(empty)(TileIndex(_))
}
