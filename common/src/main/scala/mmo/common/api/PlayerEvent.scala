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

final case class MovementAcked(position: V2[Double]) extends PlayerEvent

final case class Teleported(compactGameMap: CompactGameMap) extends PlayerEvent

final case class PlayerPositionsChanged(positions: Seq[PlayerPositionsChanged.Entry]) extends PlayerEvent

object PlayerPositionsChanged {
  final case class Entry(
    id: PlayerId,
    position: V2[Double],
    direction: Direction,
    lookDirection: LookDirection
  )
}

final case class OtherPlayerDisappeared(id: PlayerId) extends PlayerEvent

final case class OtherPlayerConnected(id: PlayerId, name: String) extends PlayerEvent

final case class OtherPlayerDisconnected(id: PlayerId) extends PlayerEvent
