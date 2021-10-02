package mmo.server.game

import mmo.common.api.{Direction, Id, LookDirection, OtherPlayerConnected, PlayerEvent, PlayerId, SessionEstablished, Teleported}
import mmo.common.linear.V2

import akka.stream.scaladsl.SourceQueueWithComplete
import scala.math.Ordering.Implicits.infixOrderingOps

class PlayerSpawnLogic(
  maps: Map[Id[ServerGameMap], ServerGameMap]
) {

  private val initialMap: ServerGameMap = maps.values.minBy(_.id.asLong)
  private val initialPosition: V2[Double] = V2(2, 1)

  def playerConnected(playerId: PlayerId, name: String, queue: SourceQueueWithComplete[PlayerEvent])(state: GameState): GameState = {
    val player = PlayerState(
      id = playerId,
      name = name,
      mapId = initialMap.id,
      position = initialPosition,
      direction = Direction.none,
      lookDirection = LookDirection.down,
      hitPoints = ServerConstants.playerMaxHitPoints,
      maxHitPoints = ServerConstants.playerMaxHitPoints,
      queue = queue,
      receivedAtNano = ServerTime.now,
      attackStartedAt = ServerTime.now,
      appearance = ServerConstants.playerAppearance
    )

    val newState = state.updatePlayer(player)
    val playerNames = newState.players.map { case (id, player) => id -> player.name }.toSeq

    queue.offer(SessionEstablished(playerId, playerNames, initialMap.compactGameMap))
    Broadcast.except(
      OtherPlayerConnected(player.id, player.name),
      player.id
    )(newState.players.values)

    Broadcast.mapEnter(player, previousMapId = None)(newState)
    newState
  }

  def respawnPlayers(state: GameState): GameState = {
    val (players, remaining) = state.playersToRespawn.partition(_._1 <= state.tick)

    players.foldLeft(state) {
      case (state, (_, playerId)) =>
        state.players.get(playerId) match {
          case None => state
          case Some(player) =>
            val respawned = player.copy(
              mapId = initialMap.id,
              position = initialPosition,
              direction = Direction.none,
              lookDirection = LookDirection.down,
              hitPoints = player.maxHitPoints
            )
            val newState = state.updatePlayer(respawned)
            respawned.queue.offer(Teleported(initialMap.compactGameMap))
            Broadcast.mapEnter(respawned, previousMapId = Some(player.mapId))(newState)
            newState
        }
    }.copy(playersToRespawn = remaining)
  }
}
