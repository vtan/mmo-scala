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

final case class EntityPositionsChanged(positions: Seq[EntityPositionsChanged.Entry]) extends PlayerEvent

object EntityPositionsChanged {
  final case class Entry(
    entityId: EntityId,
    position: V2[Double],
    direction: Direction,
    lookDirection: LookDirection
  )
}

final case class EntityAttacked(id: PlayerId) extends PlayerEvent

final case class OtherPlayerDisappeared(id: PlayerId) extends PlayerEvent

final case class OtherPlayerConnected(id: PlayerId, name: String) extends PlayerEvent

final case class OtherPlayerDisconnected(id: PlayerId) extends PlayerEvent

final case class MobsAppeared(mobs: Seq[(MobId, EntityAppearance)]) extends PlayerEvent

final case class MobDied(id: MobId) extends PlayerEvent
