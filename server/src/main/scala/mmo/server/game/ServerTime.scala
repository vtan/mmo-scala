package mmo.server.game

final case class ServerTime(asNanos: Long) extends AnyVal {

  def plusSeconds(seconds: Int): ServerTime =
    copy(asNanos = asNanos + seconds.toLong * 1_000_000_000)

  def isBefore(rhs: ServerTime): Boolean =
    asNanos < rhs.asNanos

  def -(rhs: ServerTime): ServerTime =
    ServerTime(asNanos - rhs.asNanos)

  def toSeconds: Double =
    asNanos.toDouble / 1.0e9
}

object ServerTime {
  implicit val ordering: Ordering[ServerTime] = Ordering.by(_.asNanos)

  def now: ServerTime = ServerTime(System.nanoTime())
}
