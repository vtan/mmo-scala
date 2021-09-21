package mmo.server.game

import mmo.common.api._
import mmo.common.linear.V2
import mmo.common.map.GameMap
import mmo.server.game.ServerGameMap.MobSpawn

import akka.stream.scaladsl.SourceQueueWithComplete
import java.util.UUID
import scala.concurrent.duration._
import scala.util.Random

class GameLogic(
  maps: Map[Id[ServerGameMap], ServerGameMap],
  mapNames: Map[String, Id[ServerGameMap]],
  mobTemplates: Map[String, MobTemplate]
) {

  val tickPeriod: FiniteDuration = 100.millis
  private val ticksPerSecond: Double = 1.second / tickPeriod
  private val largeTickFrequency: Int = ticksPerSecond.toInt

  private implicit val random: Random = new Random()

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
    Broadcast.except(
      OtherPlayerConnected(player.id, player.name),
      player.id
    )(newState.players.values)

    Broadcast.mapEnter(player, previousMapId = None)(newState)
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
            val newState = state.updatePlayer(newPlayer.id, newPlayer)
            Broadcast.playerPosition(newPlayer, ack = !invalid)(newState.players.values)
            newState
        }

      case PlayerCommand.Attack(target) =>
        val player = state.players(playerId)
        if ((state.serverTime - player.attackStartedAt).toSeconds > Constants.playerAttackLength) {
          Broadcast.toMapExcept(
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
      Broadcast.event(OtherPlayerDisconnected(id))(newState.players.values)
    }
    newState
  }

  def timerTicked(state: GameState): GameState = {
    val afterSmallTick = moveMobs(state)
    if (state.tick % largeTickFrequency == 0) {
      respawnMobs(afterSmallTick)
    } else {
      afterSmallTick
    }
  }

  private def moveMobs(state: GameState): GameState = {
    val mobs = state.mobs.values
      .groupBy(_.mapId)
      .flatMap {
        case (mapId, mobsOnMap) =>
          val map = maps(mapId).gameMap
          val (movedMobs, shouldBroadcast) = mobsOnMap.map { mob =>
            val moved = moveMob(map)(mob)
            val changedDirection = mob.direction != moved.direction
            val enoughTicksPassed = state.tick - mob.lastBroadcastTick >= largeTickFrequency
            moved -> (changedDirection || enoughTicksPassed)
          }.unzip
          if (shouldBroadcast.exists(identity)) {
            val mobs = movedMobs.map(_.copy(lastBroadcastTick = state.tick))
            Broadcast.toMap(EntityPositionsChanged(mobs.map(_.toPositionChange).toSeq), mapId)(state.players.values)
            mobs
          } else {
            movedMobs
          }
      }
    state.updateMobs(mobs)
  }

  private def moveMob(map: GameMap)(mob: Mob): Mob = {

    def nextPositionAlong(direction: Direction, dt: Double): V2[Double] =
      mob.position + (dt * Constants.mobTilePerSecond / ticksPerSecond) *: direction.vector

    def isIllegal(position: V2[Double]): Boolean = {
      val wouldCollide = map.doesRectCollide(mob.template.appearance.collisionBox.translate(position))
      val wouldBeFarFromSpawn = (position - mob.spawn.position).lengthSq >= 6 * 6
      wouldCollide || wouldBeFarFromSpawn
    }

    val nextPosition = nextPositionAlong(mob.direction, dt = 1)

    if (isIllegal(nextPosition)) {
      val candidates = Direction.allMoving.collect(Function.unlift { dir =>
        val pos = nextPositionAlong(dir, dt = 1)
        if (!isIllegal(pos) && !isIllegal(nextPositionAlong(dir, dt = 5))) {
          Some(dir -> pos)
        } else {
          None
        }
      })
      random.shuffle(candidates) match {
        case (direction, position) +: _ =>
          mob.copy(direction = direction, position = position)
        case _ =>
          mob.copy(direction = mob.direction.inverse)
      }
    } else if (!mob.direction.isMoving || random.nextDouble() < 0.01) {
      val newDirection = Direction.random
      mob.copy(position = nextPosition, direction = newDirection)
    } else {
      mob.copy(position = nextPosition)
    }
  }

  private def respawnMobs(state: GameState): GameState = {
    val (mobsToRespawn, remaining) = state.mobsToRespawn.partition(_._1.isBefore(state.serverTime))
    val newMobs = mobsToRespawn.map(_._2).map(spawnMob)
    newMobs.foreach { mob =>
      val event = mob.toEvent
      state.players.values
        .filter(_.mapId == mob.mapId)
        .foreach(_.queue.offer(MobsAppeared(Seq(event))))
    }
    state.copy(
      mobsToRespawn = remaining,
      mobs = state.mobs ++ newMobs.map(m => m.id -> m).toMap
    )
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
      template = template,
      spawn = mobSpawn,
      mapId = mobSpawn.mapId,
      position = mobSpawn.position,
      direction = Direction.none,
      lookDirection = LookDirection.down,
      hitPoints = template.maxHitPoints,
      lastBroadcastTick = 0
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
        val damage = 1
        val remainingHitPoints = Math.max(0, mob.hitPoints - damage)
        val entityDamaged = EntityDamaged(mob.id, damage = damage, hitPoints = remainingHitPoints)
        if (remainingHitPoints > 0) {
          Broadcast.toMap(entityDamaged, mob.mapId)(state.players.values)
          state.updateMob(mob.copy(hitPoints = remainingHitPoints))
        } else {
          Broadcast.toMap(entityDamaged, mob.mapId)(state.players.values)
          Broadcast.toMap(MobDied(mob.id), mob.mapId)(state.players.values)
          val respawnAt = state.serverTime.plusSeconds(10)
          state.copy(
            mobs = state.mobs - mob.id,
            mobsToRespawn = state.mobsToRespawn :+ (respawnAt -> mob.spawn)
          )
        }
      case None => state
    }
  }
}
