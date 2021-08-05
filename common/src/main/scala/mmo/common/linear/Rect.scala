package mmo.common.linear

final case class Rect[@specialized(Float) T](
  xy: V2[T],
  wh: V2[T]
) {
  def translate(v: V2[T])(implicit num: Numeric[T]): Rect[T] =
    copy(xy = xy + v)
}
