package mmo.server.game

import mmo.common.api.{Id, MobId, PlayerId}
import mmo.server.game.ServerGameMap.MobSpawn

import java.util.concurrent.TimeUnit

final case class GameState(
  serverTime: ServerTime,
  tick: Tick,
  lastTickAt: ServerTime,
  players: Map[PlayerId, PlayerState],
  mobs: Map[MobId, Mob],
  mobsToRespawn: Vector[(Tick, MobSpawn)],
  playersToRespawn: Vector[(Tick, PlayerId)]
) {
  lazy val fractionOfTick: Double =
    (serverTime - lastTickAt).toSeconds / Tick.tickPeriod.toUnit(TimeUnit.SECONDS)

  def alivePlayerOnMap(playerId: PlayerId, mapId: Id[ServerGameMap]): Option[PlayerState] =
    players.get(playerId).filter(p => p.mapId == mapId && p.isAlive)

  def alivePlayersOnMap(mapId: Id[ServerGameMap]): Iterable[PlayerState] =
    players.values.filter(p => p.mapId == mapId && p.isAlive)

  def updatePlayer(player: PlayerState): GameState =
    copy(players = players.updated(player.id, player))

  def updatePlayers(players: Iterable[PlayerState]): GameState =
    copy(players = this.players ++ players.map(p => p.id -> p))

  def updateMob(mob: Mob): GameState =
    copy(mobs = mobs.updated(mob.id, mob))

  def updateMobs(mobs: Iterable[Mob]): GameState =
    copy(mobs = this.mobs ++ mobs.map(m => m.id -> m))

  def removePlayer(id: PlayerId): GameState =
    copy(players = players.removed(id))

  def addPlayerRespawns(respawns: Iterable[(Tick, PlayerId)]): GameState =
    copy(playersToRespawn = playersToRespawn ++ respawns)

  def updateServerTime(): GameState =
    copy(serverTime = ServerTime.now)

  def increaseTick: GameState =
    copy(tick = tick + Tick(1), lastTickAt = serverTime)
}

object GameState {
  val empty: GameState = GameState(
    serverTime = ServerTime(Long.MinValue),
    tick = Tick(0),
    lastTickAt = ServerTime(Long.MinValue),
    players = Map.empty,
    mobs = Map.empty,
    mobsToRespawn = Vector.empty,
    playersToRespawn = Vector.empty
  )
}
