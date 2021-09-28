package mmo.server.game

import mmo.common.api._
import mmo.common.linear.V2
import mmo.common.map.GameMap
import mmo.server.game.ServerGameMap.MobSpawn

import akka.stream.scaladsl.SourceQueueWithComplete
import java.util.UUID
import scala.util.Random

class GameLogic(
  maps: Map[Id[ServerGameMap], ServerGameMap],
  mapNames: Map[String, Id[ServerGameMap]],
  mobTemplates: Map[String, MobTemplate]
) {

  private implicit val random: Random = new Random()

  private object positionConstraints {
    // TODO this is flaky
    val maxAllowedDistanceSqFromPredicted: Double = 1.5

    // Allow a diagonal tile traversal and some more
    val maxAllowedDistanceSqFromLast: Double = 2 * (1.1 * 1.1)
  }

  private val mobAttackRangeSq = 1.0

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
    val player = PlayerState(playerId, name, mapId, V2(2, 1), Direction.none, LookDirection.down, Constants.playerMaxHitPoints, Constants.playerMaxHitPoints, queue, ServerTime.now, ServerTime.now, ServerConstants.playerAppearance)

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
    val afterSmallTick = updateMobs(state)
    if (state.tick % Tick.largeTickFrequency == 0) {
      respawnMobs(afterSmallTick)
    } else {
      afterSmallTick
    }
  }

  private def updateMobs(state: GameState): GameState =
    state.mobs.values
      .groupBy(_.mapId)
      .foldLeft(state) {
        case (gs, (mapId, mobsOnMap)) =>
          val map = maps(mapId)
          updateMobsOnMap(map, mobsOnMap)(gs)
      }

  private def updateMobsOnMap(map: ServerGameMap, mobsOnMap: Iterable[Mob])(state: GameState): GameState = {
    val players = state.players.values.filter(_.mapId == map.id)

    val (movedMobs, shouldBroadcast, attackOpts) = mobsOnMap.map { mob =>
      val attack = if (state.tick >= mob.lastAttackTick + mob.template.attackCooldownTicks) {
        attackWithMob(mob, players)
      } else {
        None
      }
      val moved = moveMob(map.gameMap)(mob)
      val attacked = if (attack.isDefined) moved.copy(lastAttackTick = state.tick) else moved
      val changedDirection = mob.direction != attacked.direction
      val enoughTicksPassed = state.tick - mob.lastBroadcastTick >= Tick.largeTickFrequency
      (attacked, changedDirection || enoughTicksPassed, attack)
    }.unzip3

    val attacks = attackOpts.collect { case Some(x) => x }

    val attackEvents = attacks.map { case (mobId, _, _) => EntityAttacked(mobId) }

    val (damagedPlayers, damageEvents) = attacks
      .groupMapReduce(_._2)(_._3)(_ + _)
      .map {
        case (playerId, damage) =>
          val player = state.players(playerId)
          val newHitPoints = (player.hitPoints - damage) match {
            case n if n > 0 => n
            case _ => player.maxHitPoints
          }
          player.copy(hitPoints = newHitPoints) -> EntityDamaged(playerId, damage, newHitPoints)
      }
      .unzip

    val (tickedMobs, movementEvents) = if (shouldBroadcast.exists(identity)) {
      val ticked = movedMobs.map(_.copy(lastBroadcastTick = state.tick))
      val events = Seq(EntityPositionsChanged(ticked.map(_.toPositionChange).toSeq))
      ticked -> events
    } else {
      movedMobs -> Seq.empty
    }

    // TODO merge them into one event
    (movementEvents ++ attackEvents ++ damageEvents).foreach { event =>
      Broadcast.toMap(event, map.id)(state.players.values)
    }

    state.updateMobs(tickedMobs).updatePlayers(damagedPlayers)
  }

  private def moveMob(map: GameMap)(mob: Mob): Mob = {

    def nextPositionAlong(position: V2[Double], direction: Direction, dt: Double) : V2[Double] =
      position + (dt * Constants.mobTilePerSecond / Tick.ticksPerSecond) *: direction.vector

    def isIllegal(position: V2[Double]): Boolean = {
      val wouldCollide = map.doesRectCollide(mob.template.appearance.collisionBox.translate(position))
      val wouldBeFarFromSpawn = (position - mob.spawn.position).lengthSq >= 6 * 6
      wouldCollide || wouldBeFarFromSpawn
    }

    def respawned: Mob = mob.copy(position = mob.spawn.position, direction = Direction.none)

    def withRandomLegalDirection(position: V2[Double]): Mob = {
      val candidates = Direction.allMoving.filter { dir =>
        !isIllegal(nextPositionAlong(position, dir, dt = 1)) &&
          !isIllegal(nextPositionAlong(position, dir, dt = 3))
      }
      random.shuffle(candidates).headOption.fold(respawned) { direction =>
        mob.copy(position = position, direction = direction)
      }
    }

    // The client is already extrapolating the mob towards the next position,
    // so the next position should have already been set to a legal one at the previous tick.
    // Therefore we check if the position two ticks later is valid
    // and go ahead with the next position according to the current direction.
    val nextPosition = nextPositionAlong(mob.position, mob.direction, dt = 1)
    val nextPosition2 = nextPositionAlong(mob.position, mob.direction, dt = 2)

    if (isIllegal(nextPosition)) {
      // This branch should rarely/never happen
      respawned
    } else if (isIllegal(nextPosition2) || !mob.direction.isMoving || random.nextDouble() < 0.01) {
      withRandomLegalDirection(nextPosition)
    } else {
      mob.copy(position = nextPosition)
    }
  }

  private def attackWithMob(mob: Mob, players: Iterable[PlayerState]): Option[(MobId, PlayerId, Int)] = {
    val candidates = players.filter { player =>
      (player.collisionBoxCenter - mob.collisionBoxCenter).lengthSq <= mobAttackRangeSq
    }
    random.shuffle(candidates).headOption.map(player => (mob.id, player.id, 1))
  }

  private def respawnMobs(state: GameState): GameState = {
    // TODO: measure respawn time in ticks
    val (mobsToRespawn, remaining) = state.mobsToRespawn.partition(_._1.isBefore(state.serverTime))
    val newMobs = mobsToRespawn.map(_._2).map(spawnMob)
    newMobs.foreach { mob =>
      val event = mob.toEvent(dt = 0)
      state.players.values
        .filter(_.mapId == mob.mapId)
        .foreach(_.queue.offer(EntitiesAppeared(Seq(event))))
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
    val oldHitboxCenter = oldPosition + ServerConstants.playerCollisionBoxCenter
    val newHitboxCenter = newPosition + ServerConstants.playerCollisionBoxCenter
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
      lastBroadcastTick = 0,
      lastAttackTick = 0
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
