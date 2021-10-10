package mmo.client.game

object MathUtil {

  def clamp(value: Double, min: Double, max: Double): Double =
    if (value < min) {
      min
    } else if (value > max) {
      max
    } else {
      value
    }
}
