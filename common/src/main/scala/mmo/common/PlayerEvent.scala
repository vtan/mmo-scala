package mmo.common

import com.sksamuel.avro4s.AvroSchema
import java.util.UUID
import org.apache.avro.Schema

sealed trait PlayerEvent

object PlayerEvent {
  val avroSchema: Schema = AvroSchema[PlayerEvent]
}

final case class PlayerPositionChanged(positions: Seq[PlayerPositionChanged.Entry]) extends PlayerEvent

object PlayerPositionChanged {
  final case class Entry(id: UUID, x: Int, y: Int)
}

final case class PlayerDisconnected(id: UUID) extends PlayerEvent
