package mmo.common.linear

final case class Rect[@specialized(Int, Float, Double) T](
  xy: V2[T],
  wh: V2[T]
) {
  def x: T = xy.x
  def y: T = xy.y
  def w: T = wh.x
  def h: T = wh.y

  def end(implicit num: Numeric[T]): V2[T] =
    xy + wh

  def translate(v: V2[T])(implicit num: Numeric[T]): Rect[T] =
    copy(xy = xy + v)

  def contains(p: V2[T])(implicit num: Numeric[T]): Boolean = {
    import num._
    p.x >= x && p.x < x + w && p.y >= y && p.y < y + h
  }

  def intersects(rhs: Rect[T])(implicit num: Numeric[T]): Boolean = {
    import num._

    val V2(ax, ay) = xy
    val V2(bx, by) = rhs.xy
    val V2(axEnd, ayEnd) = end
    val V2(bxEnd, byEnd) = rhs.end

    ax < bxEnd && bx < axEnd && ay < byEnd && by < ayEnd
  }
}
