package mmo.client.game

import mmo.common.api.{Direction, LookDirection}
import mmo.common.linear.V2

final case class PlayerState(
  position: V2[Float],
  lastPositionFromServer: V2[Float],
  smoothedPositionAtLastServerUpdate: V2[Float],
  direction: Direction,
  lookDirection: LookDirection,
  receivedAt: Float
)
