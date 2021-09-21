package mmo.server.game

import mmo.common.api.{EntityPositionsChanged, Id, MobsAppeared, MovementAcked, OtherPlayerDisappeared, PlayerEvent, PlayerId}

object Broadcast {

  def playerPosition(player: PlayerState, ack: Boolean)(players: Iterable[PlayerState]): Unit = {
    val toOthers = EntityPositionsChanged(Seq(player.toEvent))
    val toPlayer = if (ack) MovementAcked(player.position) else toOthers
    player.queue.offer(toPlayer)
    this.toMapExcept(toOthers, player.mapId, except = player.id)(players)
  }

  def mapEnter(player: PlayerState, previousMapId: Option[Id[ServerGameMap]])(state: GameState): Unit =
    state.players.values.foreach { recipient =>
      val isOnTargetMap = recipient.mapId == player.mapId
      val isOnPreviousMap = previousMapId.contains(recipient.mapId)

      val event: Seq[PlayerEvent] = if (recipient.id == player.id) {
        val playersOnMap = state.players.filter(_._2.mapId == player.mapId).values
        val mobsAppeared = state.mobs.values
          .filter(_.mapId == player.mapId)
          .map(_.toEvent)

        val entities = playersOnMap.map(_.toEvent)
        List(
          MobsAppeared(mobsAppeared.toSeq),
          EntityPositionsChanged(entities.toSeq)
        )
      } else if (isOnTargetMap) {
        List(EntityPositionsChanged(Seq(player.toEvent)))
      } else if (isOnPreviousMap) {
        List(OtherPlayerDisappeared(player.id))
      } else {
        Nil
      }

      event.foreach(recipient.queue.offer)
    }

  def event(event: PlayerEvent)(players: Iterable[PlayerState]): Unit =
    players.foreach(_.queue.offer(event))

  def except(event: PlayerEvent, except: PlayerId)(players: Iterable[PlayerState]): Unit =
    this.event(event)(players.filter(_.id != except))

  def toMap(event: PlayerEvent, mapId: Id[ServerGameMap])(players: Iterable[PlayerState]): Unit =
    this.event(event)(players.filter(p => p.mapId == mapId))

  def toMapExcept(event: PlayerEvent, mapId: Id[ServerGameMap], except: PlayerId)(players: Iterable[PlayerState]): Unit =
    this.event(event)(players.filter(p => p.id != except && p.mapId == mapId))
}
