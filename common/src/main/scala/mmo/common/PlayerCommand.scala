package mmo.common

import com.sksamuel.avro4s.AvroSchema
import org.apache.avro.Schema

sealed trait PlayerCommand

object PlayerCommand {
  val avroSchema: Schema = AvroSchema[PlayerCommand]

  final case class Move(dx: Int, dy: Int) extends PlayerCommand
}
