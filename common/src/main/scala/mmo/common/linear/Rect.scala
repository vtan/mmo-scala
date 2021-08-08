package mmo.common.linear

final case class Rect[@specialized(Int, Float, Double) T](
  xy: V2[T],
  wh: V2[T]
) {
  def x: T = xy.x
  def y: T = xy.y
  def w: T = wh.x
  def h: T = wh.y

  def center(implicit frac: Fractional[T]): V2[T] =
    xy + wh / frac.fromInt(2)

  def end(implicit num: Numeric[T]): V2[T] =
    xy + wh

  def translate(v: V2[T])(implicit num: Numeric[T]): Rect[T] =
    copy(xy = xy + v)

  def contains(p: V2[T])(implicit num: Numeric[T]): Boolean = {
    import num._
    p.x >= x && p.x < x + w && p.y >= y && p.y < y + h
  }

  def contains(rhs: Rect[T])(implicit num: Numeric[T]): Boolean = {
    import num._
    rhs.x >= x && rhs.x + rhs.w <= x + w && rhs.y >= y && rhs.y + rhs.h <= y + h
  }

  def intersects(rhs: Rect[T])(implicit num: Numeric[T]): Boolean = {
    import num._

    val V2(ax, ay) = xy
    val V2(bx, by) = rhs.xy
    val V2(axEnd, ayEnd) = end
    val V2(bxEnd, byEnd) = rhs.end

    ax < bxEnd && bx < axEnd && ay < byEnd && by < ayEnd
  }

  def map[U](f: T => U): Rect[U] =
    Rect(xy.map(f), wh.map(f))
}

object Rect {
  def apply[T](x: T, y: T, w: T, h: T): Rect[T] =
    Rect(V2(x, y), V2(w, h))
}
