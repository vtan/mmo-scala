package mmo.server.game

import mmo.common.api.{MobId, PlayerId}
import mmo.server.game.ServerGameMap.MobSpawn

final case class GameState(
  serverTime: ServerTime,
  tick: Long,
  lastTickAt: ServerTime,
  players: Map[PlayerId, PlayerState],
  mobs: Map[MobId, Mob],
  mobsToRespawn: Vector[(ServerTime, MobSpawn)]
) {
  lazy val secondsSinceLastTick: Double = (serverTime - lastTickAt).toSeconds

  def updatePlayer(id: PlayerId, state: PlayerState): GameState =
    copy(players = players.updated(id, state))

  def updateMob(mob: Mob): GameState =
    copy(mobs = mobs.updated(mob.id, mob))

  def updateMobs(mobs: Iterable[Mob]): GameState =
    copy(mobs = this.mobs ++ mobs.map(m => m.id -> m))

  def removePlayer(id: PlayerId): GameState =
    copy(players = players.removed(id))

  def updateServerTime(): GameState =
    copy(serverTime = ServerTime.now)

  def increaseTick: GameState =
    copy(tick = tick + 1, lastTickAt = serverTime)
}

object GameState {
  val empty: GameState = GameState(
    serverTime = ServerTime(Long.MinValue),
    tick = 0,
    lastTickAt = ServerTime(Long.MinValue),
    players = Map.empty,
    mobs = Map.empty,
    mobsToRespawn = Vector.empty
  )
}
