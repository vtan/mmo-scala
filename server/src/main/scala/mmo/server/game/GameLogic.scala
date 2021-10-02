package mmo.server.game

import mmo.common.api._

import akka.stream.scaladsl.SourceQueueWithComplete
import java.util.UUID
import scala.util.Random

class GameLogic(
  maps: Map[Id[ServerGameMap], ServerGameMap],
  mapNames: Map[String, Id[ServerGameMap]],
  mobTemplates: Map[String, MobTemplate]
) {

  private implicit val random: Random = new Random()

  private val mobSpawnLogic = new MobSpawnLogic(mobTemplates)
  private val mobUpdateLogic = new MobUpdateLogic(maps)
  private val playerActionLogic = new PlayerActionLogic(maps, mapNames)
  private val playerSpawnLogic = new PlayerSpawnLogic(maps)

  def initialGameState: GameState = {
    val mobs = maps.values.flatMap(_.mobSpawns.map(mobSpawnLogic.spawnMob))
    GameState.empty.copy(
      serverTime = ServerTime.now,
      mobs = mobs.map(m => m.id -> m).toMap
    )
  }

  def playerConnected(playerId: PlayerId, queue: SourceQueueWithComplete[PlayerEvent])(state: GameState): GameState = {
    val name = UUID.randomUUID().toString.take(6).toUpperCase
    playerSpawnLogic.playerConnected(playerId, name, queue)(state)
  }

  def playerCommandReceived(playerId: PlayerId, command: PlayerCommand)(state: GameState): GameState =
    command match {
      case requested: PlayerCommand.Move =>
        val existing = state.players.getOrElse(playerId, throw new IllegalStateException(s"Player state missing for $playerId"))
        playerActionLogic.handlePlayerMovement(existing, requested)(state)

      case command: PlayerCommand.Attack =>
        val player = state.players.getOrElse(playerId, throw new IllegalStateException(s"Player state missing for $playerId"))
        playerActionLogic.handlePlayerAttack(player, command)(state)

      case _: PlayerCommand.InitiateSession => disconnectPlayer(playerId)(state)
      case _: PlayerCommand.Ping => disconnectPlayer(playerId)(state)
    }

  def disconnectPlayer(id: PlayerId)(state: GameState): GameState = {
    val newState = state.removePlayer(id)
    state.players.get(id).foreach { player =>
      player.queue.complete()
      Broadcast.event(OtherPlayerDisconnected(id))(newState.players.values)
    }
    newState
  }

  def timerTicked(state: GameState): GameState = {
    val afterSmallTick = mobUpdateLogic.updateMobs(state)
    if (state.tick.isLargeTick) {
      playerSpawnLogic.respawnPlayers(
        mobSpawnLogic.respawnMobs(afterSmallTick)
      )
    } else {
      afterSmallTick
    }
  }
}
