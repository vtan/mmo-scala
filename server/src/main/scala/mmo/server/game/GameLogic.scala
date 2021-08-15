package mmo.server.game

import mmo.common.api._
import mmo.common.linear.V2
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
    val player = PlayerState(playerId, name, mapId, V2(2, 1), Direction.none, LookDirection.down, queue, ServerTime.now, ServerTime.now)

    val newState = state.updatePlayer(player.id, player)
    val playerNames = newState.players.map { case (id, player) => id -> player.name }.toSeq

    queue.offer(SessionEstablished(playerId, playerNames, map.compactGameMap))
    broadcastExcept(
      OtherPlayerConnected(player.id, player.name),
      player.id
    )(newState.players.values)

    broadcastMapEnter(player, previousMapId = None)(newState)
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
          val timeElapsed = (state.serverTime - existing.receivedAtNano).toSeconds
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
              receivedAtNano = state.serverTime
            )
            val newState = state.updatePlayer(newPlayer.id, newPlayer)
            newPlayer.queue.offer(Teleported(targetMap.compactGameMap))
            broadcastMapEnter(newPlayer, Some(existing.mapId))(newState)
            newState

          case None =>
            val newPlayer = if (invalid) {
              existing.copy(
                position = if (movedFarFromPredicted) predictedPosition else existing.position,
                direction = if (movedToObstacle) Direction.none else requested.direction,
                lookDirection = requested.lookDirection,
                receivedAtNano = state.serverTime
              )
            } else {
              existing.copy(
                position = requested.position,
                direction = requested.direction,
                lookDirection = requested.lookDirection,
                receivedAtNano = state.serverTime
              )
            }
            val newState = state.updatePlayer(newPlayer.id, newPlayer)
            broadcastPlayerPosition(newPlayer, ack = !invalid)(newState.players.values)
            newState
        }

      case PlayerCommand.Attack(target) =>
        val player = state.players(playerId)
        if ((state.serverTime - player.attackStartedAt).toSeconds > Constants.playerAttackLength) {
          broadcastToMapExcept(
            EntityAttacked(playerId),
            player.mapId,
            except = player.id
          )(state.players.values)

          hitMobWithPlayer(player, target)(
            state.updatePlayer(playerId, player.copy(
              attackStartedAt = state.serverTime
            ))
          )
        } else {
          // TODO: log invalid commands?
          state
        }

      case _: PlayerCommand.InitiateSession => disconnectPlayer(playerId)(state)
      case _: PlayerCommand.Ping => disconnectPlayer(playerId)(state)
    }

  def disconnectPlayer(id: PlayerId)(state: GameState): GameState = {
    val newState = state.removePlayer(id)
    state.players.get(id).foreach { player =>
      player.queue.complete()
      broadcastEvent(OtherPlayerDisconnected(id))(newState.players.values)
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

  private def broadcastPlayerPosition(player: PlayerState, ack: Boolean)(players: Iterable[PlayerState]): Unit = {
    val toOthers = EntityPositionsChanged(Seq(playerStateToEvent(player)))
    val toPlayer = if (ack) MovementAcked(player.position) else toOthers
    player.queue.offer(toPlayer)
    broadcastToMapExcept(toOthers, player.mapId, except = player.id)(players)
  }

  private def broadcastMapEnter(player: PlayerState, previousMapId: Option[Id[ServerGameMap]])(state: GameState): Unit =
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

  private def broadcastEvent(event: PlayerEvent)(players: Iterable[PlayerState]): Unit =
    players.foreach(_.queue.offer(event))

  private def broadcastExcept(event: PlayerEvent, except: PlayerId)(players: Iterable[PlayerState]): Unit =
    broadcastEvent(event)(players.filter(_.id != except))

  private def broadcastToMap(event: PlayerEvent, mapId: Id[ServerGameMap])(players: Iterable[PlayerState]): Unit =
    broadcastEvent(event)(players.filter(p => p.mapId == mapId))

  private def broadcastToMapExcept(event: PlayerEvent, mapId: Id[ServerGameMap], except: PlayerId)(players: Iterable[PlayerState]): Unit =
    broadcastEvent(event)(players.filter(p => p.id != except && p.mapId == mapId))

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

  private def hitMobWithPlayer(player: PlayerState, target: V2[Double])(state: GameState): GameState = {
    val hitMob = state.mobs.values
      .filter(_.mapId == player.mapId)
      .minByOption { mob =>
        val spriteCenter = mob.position + mob.template.appearance.spriteCenter
        (spriteCenter - target).lengthSq
      }
      .filter { mob =>
        val collisionCenter = mob.position + mob.template.appearance.collisionCenter
        val playerCenter = player.position + Constants.playerHitboxCenter
        val attack = collisionCenter - playerCenter
        attack.lengthSq < Constants.playerAttackRangeSq
      }
    hitMob match {
      case Some(mob) =>
        broadcastToMap(MobDisappeared(mob.id), mob.mapId)(state.players.values)
        val respawnAt = state.serverTime.plusSeconds(10)
        state.copy(
          mobs = state.mobs - mob.id,
          mobsToRespawn = state.mobsToRespawn :+ (respawnAt -> mob.spawn)
        )
      case None => state
    }
  }
}
