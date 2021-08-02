package mmo.common.api

import mmo.common.linear.V2

import com.sksamuel.avro4s.AvroSchema
import java.util.UUID
import org.apache.avro.Schema

sealed trait PlayerEvent

object PlayerEvent {
  val avroSchema: Schema = AvroSchema[PlayerEvent]
}

final case class SessionEstablished(id: UUID) extends PlayerEvent

final case class PlayerPositionChanged(positions: Seq[PlayerPositionChanged.Entry]) extends PlayerEvent

object PlayerPositionChanged {
  final case class Entry(id: UUID, position: V2[Float], direction: Direction)
}

final case class PlayerDisconnected(id: UUID) extends PlayerEvent
