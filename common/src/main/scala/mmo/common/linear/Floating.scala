package mmo.common.linear

trait Floating[T] {
  def sqrt(a: T): T
}

object Floating {
  implicit final class FloatingOps[T](a: T)(implicit floating: Floating[T]) {
    def sqrt: T = floating.sqrt(a)
  }

  implicit val floatFloating: Floating[Float] = new Floating[Float] {
    override def sqrt(a: Float): Float = Math.sqrt(a.toDouble).toFloat
  }

  implicit val doubleFloating: Floating[Double] = new Floating[Double] {
    override def sqrt(a: Double): Double = Math.sqrt(a)
  }
}
