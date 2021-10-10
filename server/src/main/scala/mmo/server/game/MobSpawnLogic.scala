package mmo.server.game

import mmo.common.api.{Direction, EntitiesAppeared, MobId}
import mmo.server.game.ServerGameMap.MobSpawn

import scala.math.Ordering.Implicits.infixOrderingOps

class MobSpawnLogic(
  mobTemplates: Map[String, MobTemplate]
) {

  def respawnMobs(state: GameState): GameState = {
    val (mobsToRespawn, remaining) = state.mobsToRespawn.partition(_._1 <= state.tick)
    val newMobs = mobsToRespawn.map(_._2).map(spawnMob)
    newMobs.foreach { mob =>
      val event = mob.toEvent(fractionOfTick = 0)
      Broadcast.toMap(EntitiesAppeared(Seq(event)), mob.mapId)(state.players.values)
    }
    state.copy(
      mobsToRespawn = remaining,
      mobs = state.mobs ++ newMobs.map(m => m.id -> m).toMap
    )
  }

  def spawnMob(mobSpawn: MobSpawn): Mob = {
    val template = mobTemplates.getOrElse(
      mobSpawn.templateName,
      throw new IllegalStateException(s"Cannot resolve mob template name: ${mobSpawn.templateName}")
    )
    Mob(
      id = MobId.nextId(),
      template = template,
      spawn = mobSpawn,
      mapId = mobSpawn.mapId,
      position = mobSpawn.position,
      direction = Direction.none,
      nextPosition = mobSpawn.position,
      hitPoints = template.maxHitPoints,
      lastBroadcastTick = Tick(0),
      attackTarget = None,
      lastAttackTick = Tick(0),
      damagedBy = Set.empty
    )
  }
}
