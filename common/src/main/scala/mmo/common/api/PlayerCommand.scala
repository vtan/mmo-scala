package mmo.common.api

import mmo.common.linear.V2

import com.sksamuel.avro4s.AvroSchema
import org.apache.avro.Schema

sealed trait PlayerCommand

object PlayerCommand {
  val avroSchema: Schema = AvroSchema[PlayerCommand]

  final case class InitiateSession(magic: Long = InitiateSession.magic) extends PlayerCommand

  final case class Ping(clientTimeNano: Long) extends PlayerCommand

  object InitiateSession {
    val magic: Long = 0x49c1_68b6_ec74_9f9dL
  }

  final case class Move(
    position: V2[Float],
    direction: Direction,
    lookDirection: LookDirection
  ) extends PlayerCommand
}
