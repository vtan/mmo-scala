package mmo.common.linear

final case class V2[@specialized(Int, Float, Double) T](x: T, y: T) {

  def +(rhs: V2[T])(implicit num: Numeric[T]): V2[T] = {
    import num._
    V2(x + rhs.x, y + rhs.y)
  }

  def -(rhs: V2[T])(implicit num: Numeric[T]): V2[T] = {
    import num._
    V2(x - rhs.x, y - rhs.y)
  }

  def *(rhs: V2[T])(implicit num: Numeric[T]): V2[T] = {
    import num._
    V2(x * rhs.x, y * rhs.y)
  }

  def /(rhs: V2[T])(implicit frac: Fractional[T]): V2[T] = {
    import frac._
    V2(x / rhs.x, y / rhs.y)
  }

  def *:(scalar: T)(implicit num: Numeric[T]): V2[T] = {
    import num._
    V2(scalar * x, scalar * y)
  }

  def dot(rhs: V2[T])(implicit num: Numeric[T]): T = {
    import num._
    x * rhs.x + y * rhs.y
  }

  def lengthSq(implicit num: Numeric[T]): T =
    this dot this

  def length(implicit num: Numeric[T], fl: Floating[T]): T =
    fl.sqrt(this dot this)

  def map[U](f: T => U): V2[U] =
    V2(f(x), f(y))

  def zipWith[U, V](rhs: V2[U])(f: (T, U) => V): V2[V] =
    V2(f(x, rhs.x), f(y, rhs.y))

  def normalize(implicit frac: Fractional[T], fl: Floating[T]): V2[T] = {
    import frac._
    (frac.fromInt(1) / length) *: this
  }
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
