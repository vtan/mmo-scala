package mmo.server.game

import mmo.common.api.{Direction, Id, LookDirection, PlayerEvent, PlayerId}
import mmo.common.linear.V2

import akka.stream.scaladsl.SourceQueueWithComplete

final case class PlayerState(
  id: PlayerId,
  name: String,
  mapId: Id[ServerGameMap],
  position: V2[Double],
  direction: Direction,
  lookDirection: LookDirection,
  queue: SourceQueueWithComplete[PlayerEvent],
  receivedAtNano: ServerTime,
  attackStartedAt: ServerTime
)
