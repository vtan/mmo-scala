package mmo.common.api

import mmo.common.linear.V2

import com.sksamuel.avro4s.AvroSchema
import org.apache.avro.Schema

sealed trait PlayerEvent

object PlayerEvent {
  val avroSchema: Schema = AvroSchema[PlayerEvent]
}

final case class SessionEstablished(
  playerId: PlayerId,
  players: Seq[(PlayerId, String)],
  compactGameMap: CompactGameMap
) extends PlayerEvent

final case class Pong(clientTimeNanos: Long) extends PlayerEvent

final case class Teleported(
  position: V2[Double],
  compactGameMap: CompactGameMap
) extends PlayerEvent

final case class PlayerPositionsChanged(positions: Seq[PlayerPositionsChanged.Entry]) extends PlayerEvent

object PlayerPositionsChanged {
  final case class Entry(
    id: PlayerId,
    position: V2[Double],
    direction: Direction,
    lookDirection: LookDirection,
    force: Boolean
  )
}

final case class PlayerDisappeared(id: PlayerId) extends PlayerEvent

final case class PlayerConnected(id: PlayerId, name: String) extends PlayerEvent

final case class PlayerDisconnected(id: PlayerId) extends PlayerEvent
