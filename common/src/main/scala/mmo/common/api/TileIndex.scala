package mmo.common.api

final case class TileIndex(asInt: Int) extends AnyVal {

  def isEmpty: Boolean = asInt < 0

  def toOption: Option[Int] =
    if (isEmpty) {
      None
    } else {
      Some(asInt)
    }
}

object TileIndex {
  val empty: TileIndex = TileIndex(-1)

  def fromOption(int: Option[Int]): TileIndex =
    int.fold(empty)(TileIndex(_))
}
