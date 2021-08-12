package mmo.server.game

import mmo.common.api.{Id, LookDirection, MobId}
import mmo.common.linear.V2

final case class Mob(
  id: MobId,
  mapId: Id[ServerGameMap],
  position: V2[Double],
  lookDirection: LookDirection,
  template: MobTemplate
)
