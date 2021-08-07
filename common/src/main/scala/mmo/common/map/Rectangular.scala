package mmo.common.map

trait Rectangular {
  val width: Int
  val height: Int

  def offsetOf(x: Int, y: Int): Option[Int] =
    if (x >= 0 && x < width && y >= 0 && y < height) {
      Some(x + y * width)
    } else {
      None
    }
}
