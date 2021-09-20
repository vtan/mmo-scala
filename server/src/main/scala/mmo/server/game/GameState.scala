package mmo.server.game

import mmo.common.api.{MobId, PlayerId}
import mmo.server.game.ServerGameMap.MobSpawn

final case class GameState(
  serverTime: ServerTime,
  players: Map[PlayerId, PlayerState],
  mobs: Map[MobId, Mob],
  mobsToRespawn: Vector[(ServerTime, MobSpawn)]
) {

  def updatePlayer(id: PlayerId, state: PlayerState): GameState =
    copy(players = players.updated(id, state))

  def updateMob(mob: Mob): GameState =
    copy(mobs = mobs.updated(mob.id, mob))

  def removePlayer(id: PlayerId): GameState =
    copy(players = players.removed(id))

  def updateServerTime(): GameState =
    copy(serverTime = ServerTime.now)
}

object GameState {
  val empty: GameState = GameState(
    serverTime = ServerTime(Long.MinValue),
    players = Map.empty,
    mobs = Map.empty,
    mobsToRespawn = Vector.empty
  )
}
