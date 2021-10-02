package mmo.server.game

import io.circe.Decoder
import scala.concurrent.duration._

final case class Tick(asLong: Long) extends AnyVal {

  def +(other: Tick): Tick = Tick(asLong + other.asLong)
  def -(other: Tick): Tick = Tick(asLong - other.asLong)

  def isLargeTick: Boolean =
    asLong % Tick.largeTickFrequency.asLong == 0
}

object Tick {

  val tickPeriod: FiniteDuration = 100.millis

  val ticksPerSecond: Double = 1.second / tickPeriod

  val largeTickFrequency: Tick = Tick(ticksPerSecond.toLong)

  def fromSeconds(seconds: Double): Tick =
    Tick((seconds * ticksPerSecond).toLong)

  implicit val ordering: Ordering[Tick] = Ordering.by(_.asLong)

  implicit val decoder: Decoder[Tick] = Decoder.decodeLong.map(Tick.apply)
}
