package mmo.server.game

import mmo.common.api.EntityAppearance

final case class MobTemplate(
  name: String,
  appearance: EntityAppearance,
  maxHitPoints: Int,
  attackCooldownTicks: Tick,
  aggroRadius: Double,
  idleSpeed: Double,
  aggroSpeed: Double
) {
  val aggroRadiusSq: Double = aggroRadius * aggroRadius
}
