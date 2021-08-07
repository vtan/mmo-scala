package mmo.common.api

import mmo.common.linear.V2

import com.sksamuel.avro4s.AvroSchema
import java.util.UUID
import org.apache.avro.Schema

sealed trait PlayerEvent

object PlayerEvent {
  val avroSchema: Schema = AvroSchema[PlayerEvent]
}

final case class SessionEstablished(
  id: UUID,
  gameMap: GameMap
) extends PlayerEvent

final case class Pong(clientTimeNanos: Long) extends PlayerEvent

final case class PlayerPositionsChanged(positions: Seq[PlayerPositionsChanged.Entry]) extends PlayerEvent

object PlayerPositionsChanged {
  final case class Entry(
    id: UUID,
    position: V2[Double],
    direction: Direction,
    lookDirection: LookDirection,
    force: Boolean
  )
}

final case class PlayerDisconnected(id: UUID) extends PlayerEvent
