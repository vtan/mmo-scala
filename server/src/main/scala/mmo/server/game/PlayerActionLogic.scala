package mmo.server.game

import mmo.common.api.{Constants, Direction, EntityAttacked, EntityDamaged, EntityDied, Id, PlayerCommand, StatsChanged, Teleported}
import mmo.common.linear.V2

class PlayerActionLogic(
  maps: Map[Id[ServerGameMap], ServerGameMap],
  mapNames: Map[String, Id[ServerGameMap]]
) {

  private object positionConstraints {
    // TODO this is flaky
    val maxAllowedDistanceSqFromPredicted: Double = 1.5

    // Allow a diagonal tile traversal and some more
    val maxAllowedDistanceSqFromLast: Double = 2 * (1.1 * 1.1)
  }

  def handlePlayerMovement(existing: PlayerState, requested: PlayerCommand.Move)(state: GameState): GameState =
    if (existing.isAlive) {
      val predictedPosition = {
        val timeElapsed = (state.serverTime - existing.receivedAtNano).toSeconds
        existing.position + timeElapsed *: existing.direction.vector
      }
      val map = maps(existing.mapId)
      val movedToObstacle = map.gameMap.doesRectCollide(ServerConstants.playerCollisionBox.translate(requested.position))
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
          val newState = state.updatePlayer(newPlayer)
          newPlayer.queue.offer(Teleported(targetMap.compactGameMap))
          Broadcast.mapEnter(newPlayer, Some(existing.mapId))(newState)
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
          val newState = state.updatePlayer(newPlayer)
          Broadcast.playerPosition(newPlayer, ack = !invalid)(newState.players.values)
          newState
      }
    } else {
      state
    }

  private def findTeleportAt(
    oldPosition: V2[Double],
    newPosition: V2[Double],
    teleports: Seq[ServerGameMap.Teleport]
  ): Option[(ServerGameMap.Teleport, ServerGameMap)] = {
    val oldHitboxCenter = oldPosition + ServerConstants.playerCollisionBoxCenter
    val newHitboxCenter = newPosition + ServerConstants.playerCollisionBoxCenter
    for {
      teleport <- teleports.find(tp => tp.rect.contains(newHitboxCenter) && !tp.rect.contains(oldHitboxCenter))
      mapId <- mapNames.get(teleport.targetMapName)
      map <- maps.get(mapId)
    } yield (teleport, map)
  }

  def handlePlayerAttack(player: PlayerState, command: PlayerCommand.Attack)(state: GameState): GameState =
    if ((state.serverTime - player.attackStartedAt).toSeconds > Constants.playerAttackLength) {
      Broadcast.toMapExcept(
        EntityAttacked(player.id),
        player.mapId,
        except = player.id
      )(state.players.values)

      hitMobWithPlayer(player, command.target)(
        state.updatePlayer(player.copy(
          attackStartedAt = state.serverTime
        ))
      )
    } else {
      // TODO: log invalid commands?
      state
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
        val playerCenter = player.position + ServerConstants.playerCollisionBoxCenter
        val attack = collisionCenter - playerCenter
        attack.lengthSq < Constants.playerAttackRangeSq
      }
    hitMob match {
      case Some(mob) =>
        val damage = 1
        val remainingHitPoints = Math.max(0, mob.hitPoints - damage)
        val entityDamaged = EntityDamaged(mob.id, damage = damage, hitPoints = remainingHitPoints)
        if (remainingHitPoints > 0) {
          Broadcast.toMap(entityDamaged, mob.mapId)(state.players.values)
          state.updateMob(mob.copy(
            hitPoints = remainingHitPoints,
            damagedBy = mob.damagedBy + player.id
          ))
        } else {
          Broadcast.toMap(entityDamaged, mob.mapId)(state.players.values)
          Broadcast.toMap(EntityDied(mob.id), mob.mapId)(state.players.values)

          val xpAwarded = 10
          val playersWithXp = (mob.damagedBy + player.id).toSeq.flatMap { playerId =>
            state.players.get(playerId).map { player =>
              player.copy(stats = player.stats.copy(xp = player.stats.xp + xpAwarded))
            }
          }
          playersWithXp.foreach { player =>
            player.queue.offer(StatsChanged(xp = Some(player.stats.xp)))
          }

          val respawnAt = state.tick + ServerConstants.mobRespawnTime
          state.copy(
            mobs = state.mobs - mob.id,
            mobsToRespawn = state.mobsToRespawn :+ (respawnAt -> mob.spawn),
          ).updatePlayers(playersWithXp)
        }
      case None => state
    }
  }
}
