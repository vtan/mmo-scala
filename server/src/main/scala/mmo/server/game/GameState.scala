package mmo.server.game

import mmo.common.api.PlayerId

final case class GameState(
  players: Map[PlayerId, PlayerState]
) {

  def updatePlayer(id: PlayerId, state: PlayerState): GameState =
    copy(players = players.updated(id, state))

  def removePlayer(id: PlayerId): GameState =
    copy(players = players.removed(id))
}

object GameState {
  val empty: GameState = GameState(
    players = Map.empty
  )
}
