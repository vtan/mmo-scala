package mmo.server.game

import mmo.common.api._
import mmo.common.linear.{Rect, V2}
import mmo.server.game.ServerGameMap.MobSpawn

import akka.stream.scaladsl.SourceQueueWithComplete
import java.util.UUID

class GameLogic(
  maps: Map[Id[ServerGameMap], ServerGameMap],
  mapNames: Map[String, Id[ServerGameMap]],
  mobTemplates: Map[String, MobTemplate]
) {

  private object positionConstraints {
    // TODO this is flaky
    val maxAllowedDistanceSqFromPredicted: Double = 1.5

    // Allow a diagonal tile traversal and some more
    val maxAllowedDistanceSqFromLast: Double = 2 * (1.1 * 1.1)
  }

  def initialGameState: GameState = {
    val mobs = maps.values.flatMap(_.mobSpawns.map(spawnMob))
    GameState.empty.copy(
      serverTime = ServerTime.now,
      mobs = mobs.map(m => m.id -> m).toMap
    )
  }

  def playerConnected(playerId: PlayerId, queue: SourceQueueWithComplete[PlayerEvent])(state: GameState): GameState = {
    val name = UUID.randomUUID().toString.take(6).toUpperCase
    val (mapId, map) = maps.minBy(_._1.asLong)
    val player = PlayerState(playerId, name, mapId, V2(2, 1), Direction.none, LookDirection.down, queue, System.nanoTime())

    val newState = state.updatePlayer(player.id, player)
    val playerNames = newState.players.map { case (id, player) => id -> player.name }.toSeq

    queue.offer(SessionEstablished(playerId, playerNames, map.compactGameMap))
    broadcastPlayerConnection(player, state.players.values)

    broadcastMapEnter(player, previousMapId = None, newState)
    queue.offer(EntityPositionsChanged(
      state.players.values.filter(_.mapId == mapId).map(playerStateToEvent).toSeq
    ))
    newState
  }

  def playerCommandReceived(playerId: PlayerId, command: PlayerCommand)(state: GameState): GameState =
    command match {
      case requested: PlayerCommand.Move =>
        val existing = state.players.getOrElse(playerId, throw new IllegalStateException(s"Player state missing for $playerId"))
        val predictedPosition = {
          val timeElapsed = (System.nanoTime() - existing.receivedAtNano).toDouble * 1e-9
          existing.position + timeElapsed *: existing.direction.vector
        }
        val map = maps(existing.mapId)
        val movedToObstacle = map.gameMap.doesRectCollide(Constants.playerHitbox.translate(requested.position))
        val movedFarFromLastPosition = (requested.position - existing.position).lengthSq >= positionConstraints.maxAllowedDistanceSqFromLast
        val movedFarFromPredicted = (predictedPosition - requested.position).lengthSq >= positionConstraints.maxAllowedDistanceSqFromPredicted
        val invalid = movedToObstacle || movedFarFromLastPosition || movedFarFromPredicted

        val enteredTeleport = if (!invalid) {
          findTeleportAt(existing.position, requested.position, map.teleports)
        } else {
          None
        }

        enteredTeleport match {
          case Some((teleport, targetMap)) =>
            val newPlayer = existing.copy(
              mapId = targetMap.id,
              position = teleport.targetPosition,
              direction = Direction.none,
              receivedAtNano = System.nanoTime()
            )
            val newState = state.updatePlayer(newPlayer.id, newPlayer)
            newPlayer.queue.offer(Teleported(targetMap.compactGameMap))
            broadcastMapEnter(newPlayer, Some(existing.mapId), newState)
            newState

          case None =>
            val newPlayer = if (invalid) {
              existing.copy(
                position = if (movedFarFromPredicted) predictedPosition else existing.position,
                direction = if (movedToObstacle) Direction.none else requested.direction,
                lookDirection = requested.lookDirection,
                receivedAtNano = System.nanoTime()
              )
            } else {
              existing.copy(
                position = requested.position,
                direction = requested.direction,
                lookDirection = requested.lookDirection,
                receivedAtNano = System.nanoTime()
              )
            }
            val newState = killMobWithPlayer(newPlayer)(state.updatePlayer(newPlayer.id, newPlayer))
            broadcastPlayerPosition(newPlayer, newState.players.values, ack = !invalid)
            newState
        }

      case _: PlayerCommand.InitiateSession => disconnectPlayer(playerId)(state)
      case _: PlayerCommand.Ping => disconnectPlayer(playerId)(state)
    }

  def disconnectPlayer(id: PlayerId)(state: GameState): GameState = {
    val newState = state.removePlayer(id)
    state.players.get(id).foreach { player =>
      player.queue.complete()
      broadcastEvent(OtherPlayerDisconnected(id), newState.players.values)
    }
    newState
  }

  def timerTicked(state: GameState): GameState = {
    val (mobsToRespawn, remaining) = state.mobsToRespawn.partition(_._1.isBefore(state.serverTime))
    val newMobs = mobsToRespawn.map(_._2).map(spawnMob)
    newMobs.foreach { mob =>
      val (positionChange, appearance) = newMobToEvents(mob)
      state.players.values
        .filter(_.mapId == mob.mapId)
        .foreach { player =>
          player.queue.offer(MobsAppeared(Seq(appearance)))
          player.queue.offer(EntityPositionsChanged(Seq(positionChange)))
        }
    }
    state.copy(
      mobsToRespawn = remaining,
      mobs = state.mobs ++ newMobs.map(m => m.id -> m).toMap
    )
  }

  private def broadcastPlayerPosition(player: PlayerState, players: Iterable[PlayerState], ack: Boolean): Unit = {
    val toOthers = EntityPositionsChanged(Seq(playerStateToEvent(player)))
    val toPlayer = if (ack) MovementAcked(player.position) else toOthers
    player.queue.offer(toPlayer)
    broadcastEvent(toOthers, players.filter(p => p.id != player.id && p.mapId == player.mapId))
  }

  private def broadcastPlayerConnection(player: PlayerState, players: Iterable[PlayerState]): Unit = {
    val event = OtherPlayerConnected(player.id, player.name)
    broadcastEvent(event, players.filter(_.id != player.id))
  }

  private def broadcastMapEnter(player: PlayerState, previousMapId: Option[Id[ServerGameMap]], state: GameState): Unit =
    state.players.values.foreach { recipient =>
      val isOnTargetMap = recipient.mapId == player.mapId
      val isOnPreviousMap = previousMapId.contains(recipient.mapId)

      val event: Seq[PlayerEvent] = if (recipient.id == player.id) {
        val playersOnMap = state.players.filter(_._2.mapId == player.mapId).values
        val (mobPositions, mobAppearances) = state.mobs.values
          .filter(_.mapId == player.mapId)
          .map(newMobToEvents)
          .unzip

        val entities = playersOnMap.map(playerStateToEvent) ++ mobPositions
        List(
          MobsAppeared(mobAppearances.toSeq),
          EntityPositionsChanged(entities.toSeq)
        )
      } else if (isOnTargetMap) {
        List(EntityPositionsChanged(Seq(playerStateToEvent(player))))
      } else if (isOnPreviousMap) {
        List(OtherPlayerDisappeared(player.id))
      } else {
        Nil
      }

      event.foreach(recipient.queue.offer)
    }

  private def broadcastEvent(event: PlayerEvent, players: Iterable[PlayerState]): Unit =
    players.foreach(_.queue.offer(event))

  private def playerStateToEvent(player: PlayerState): EntityPositionsChanged.Entry =
    EntityPositionsChanged.Entry(player.id, player.position, player.direction, player.lookDirection)

  private def newMobToEvents(mob: Mob): (EntityPositionsChanged.Entry, (MobId, EntityAppearance)) = {
    val positionChanged = EntityPositionsChanged.Entry(
      entityId = mob.id,
      position = mob.position,
      lookDirection = mob.lookDirection,
      direction = Direction.none
    )
    (positionChanged, mob.id -> mob.template.appearance)
  }

  private def findTeleportAt(
    oldPosition: V2[Double],
    newPosition: V2[Double],
    teleports: Seq[ServerGameMap.Teleport]
  ): Option[(ServerGameMap.Teleport, ServerGameMap)] = {
    val oldHitboxCenter = oldPosition + Constants.playerHitboxCenter
    val newHitboxCenter = newPosition + Constants.playerHitboxCenter
    for {
      teleport <- teleports.find(tp => tp.rect.contains(newHitboxCenter) && !tp.rect.contains(oldHitboxCenter))
      mapId <- mapNames.get(teleport.targetMapName)
      map <- maps.get(mapId)
    } yield (teleport, map)
  }

  private def spawnMob(mobSpawn: MobSpawn) = {
    val template = mobTemplates.getOrElse(
      mobSpawn.templateName,
      throw new IllegalStateException(s"Cannot resolve mob template name: ${mobSpawn.templateName}")
    )
    Mob(
      id = MobId.nextId(),
      mapId = mobSpawn.mapId,
      position = mobSpawn.position,
      lookDirection = LookDirection.down,
      template = template,
      spawn = mobSpawn
    )
  }

  private def killMobWithPlayer(player: PlayerState)(state: GameState): GameState = {
    val (killed, remaining) = state.mobs.partition {
      case (_, mob) =>
        player.mapId == mob.mapId &&
          Constants.playerHitbox.translate(player.position).intersects(Rect(mob.position, V2(1, 1)))
    }
    killed.values.foreach { mob =>
      broadcastEvent(MobDisappeared(mob.id), state.players.values.filter(_.mapId == player.mapId))
    }
    state.copy(
      mobs = remaining,
      mobsToRespawn = state.mobsToRespawn ++ (killed.values.map(state.serverTime.plusSeconds(5) -> _.spawn).toVector)
    )
  }
}
