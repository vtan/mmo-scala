package mmo.server.game

import scala.concurrent.duration._

object Tick {

  val tickPeriod: FiniteDuration = 100.millis

  val ticksPerSecond: Double = 1.second / tickPeriod

  val largeTickFrequency: Int = ticksPerSecond.toInt

  def fromSeconds(seconds: Double): Long = (seconds * ticksPerSecond).toLong
}
