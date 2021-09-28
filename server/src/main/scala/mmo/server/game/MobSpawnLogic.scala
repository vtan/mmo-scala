package mmo.server.game

import mmo.common.api.{Direction, EntitiesAppeared, LookDirection, MobId}
import mmo.server.game.ServerGameMap.MobSpawn

class MobSpawnLogic(
  mobTemplates: Map[String, MobTemplate]
) {

  def respawnMobs(state: GameState): GameState = {
    // TODO: measure respawn time in ticks
    val (mobsToRespawn, remaining) = state.mobsToRespawn.partition(_._1.isBefore(state.serverTime))
    val newMobs = mobsToRespawn.map(_._2).map(spawnMob)
    newMobs.foreach { mob =>
      val event = mob.toEvent(dt = 0)
      state.players.values
        .filter(_.mapId == mob.mapId)
        .foreach(_.queue.offer(EntitiesAppeared(Seq(event))))
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
      lookDirection = LookDirection.down,
      hitPoints = template.maxHitPoints,
      lastBroadcastTick = 0,
      lastAttackTick = 0
    )
  }
}
