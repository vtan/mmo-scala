package mmo.common.api

import mmo.common.linear.V2

import com.sksamuel.avro4s.AvroSchema
import org.apache.avro.Schema

sealed trait PlayerCommand

object PlayerCommand {
  val avroSchema: Schema = AvroSchema[PlayerCommand]

  final case class Move(position: V2[Float]) extends PlayerCommand
}
