package mmo.client.common

final case class V2[T](x: T, y: T) {

  def +(rhs: V2[T])(implicit num: Numeric[T]): V2[T] =
    V2(num.plus(x, rhs.x), num.plus(y, rhs.y))

  def -(rhs: V2[T])(implicit num: Numeric[T]): V2[T] =
    V2(num.minus(x, rhs.x), num.minus(y, rhs.y))

  def *(rhs: V2[T])(implicit num: Numeric[T]): V2[T] =
    V2(num.times(x, rhs.x), num.times(y, rhs.y))

  def /(rhs: V2[T])(implicit frac: Fractional[T]): V2[T] =
    V2(frac.div(x, rhs.x), frac.div(y, rhs.y))

  def *:(scalar: T)(implicit num: Numeric[T]): V2[T] =
    V2(num.times(scalar, x), num.times(scalar, y))

  def dot(rhs: V2[T])(implicit num: Numeric[T]): T =
    num.plus(num.times(x, rhs.x), num.times(y, rhs.y))

  def lengthSq(implicit num: Numeric[T]): T =
    this dot this

  def map[U](f: T => U): V2[U] =
    V2(f(x), f(y))
}

object V2 {

  def zero[T](implicit num: Numeric[T]): V2[T] =
    V2(num.zero, num.zero)

  def unitWithAngle(angle: Double): V2[Double] =
    V2(Math.cos(angle), Math.sin(angle))

  def rotate(v: V2[Double], angle: Double): V2[Double] = {
    val sin = Math.sin(angle)
    val cos = Math.cos(angle)
    V2(v.x * cos - v.y * sin, v.x * sin + v.y * cos)
  }

  def angle(v: V2[Double]): Double =
    Math.atan2(v.y, v.x)
}
