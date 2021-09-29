package mmo.server.game

import mmo.common.api.{Direction, EntityAttacked, EntityDamaged, EntityPositionsChanged, Id, MobId, PlayerId}
import mmo.common.linear.V2
import mmo.common.map.GameMap

import scala.util.Random

class MobUpdateLogic(
  maps: Map[Id[ServerGameMap], ServerGameMap],
)(
  implicit random: Random
) {

  private val mobAttackRangeSq = 1.0
  private val maxDistanceFromSpawnSq = 6.0 * 6.0

  def updateMobs(state: GameState): GameState =
    state.mobs.values
      .groupBy(_.mapId)
      .foldLeft(state) {
        case (gs, (mapId, mobsOnMap)) =>
          val map = maps(mapId)
          updateMobsOnMap(map, mobsOnMap)(gs)
      }

  private def updateMobsOnMap(map: ServerGameMap, mobsOnMap: Iterable[Mob])(state: GameState): GameState = {

    val (movedMobs, shouldBroadcast, attackOpts) = mobsOnMap.map { mob =>
      val attackTarget = mob.attackTarget
        .flatMap(state.players.get)
        .filter(_.mapId == mob.mapId)
      val attack = attackIfLegal(mob, attackTarget, state)
      val newMob = Function.chain(Seq(
        seekTarget(state),
        moveMob(map.gameMap, attackTarget),
        if (attack.isDefined) {
          (m: Mob) => m.copy(lastAttackTick = state.tick)
        } else {
          identity[Mob]
        }
      ))(mob)

      val movementChanged = mob.direction != newMob.direction || mob.speed != newMob.speed
      val enoughTicksPassed = state.tick - mob.lastBroadcastTick >= Tick.largeTickFrequency
      (newMob, movementChanged || enoughTicksPassed, attack)
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

  private def attackIfLegal(mob: Mob, attackTarget: Option[PlayerState], state: GameState): Option[(MobId, PlayerId, Int)] =
    if (state.tick >= mob.lastAttackTick + mob.template.attackCooldownTicks) {
      attackTarget.flatMap { target =>
        val distanceSq = (target.collisionBoxCenter - mob.collisionBoxCenter).lengthSq
        if (distanceSq <= mobAttackRangeSq) {
          Some((mob.id, target.id, 1))
        } else {
          None
        }
      }
    } else {
      None
    }

  private def seekTarget(state: GameState)(mob: Mob): Mob = {
    val candidates = state.players.values
      .filter(_.mapId == mob.mapId)
      .collect(Function.unlift { player =>
        val playerPosition = player.collisionBoxCenter
        val playerDistanceSq = (playerPosition - mob.collisionBoxCenter).lengthSq
        val isInAggroRadius = playerDistanceSq <= mob.template.aggroRadiusSq
        val isNearEnoughSpawn = (playerPosition - mob.spawn.position).lengthSq <= maxDistanceFromSpawnSq
        if (isInAggroRadius && isNearEnoughSpawn) {
          Some(player -> playerDistanceSq)
        } else {
          None
        }
      })
    mob.attackTarget match {
      case Some(targetId) if candidates.exists(_._1.id == targetId) =>
        mob
      case _ =>
        val nearestCandidate = candidates
          .minByOption { case (_, playerDistanceSq) => playerDistanceSq }
          .map(_._1)
        mob.copy(attackTarget = nearestCandidate.map(_.id))
    }
  }

  private def moveMob(map: GameMap, attackTarget: Option[PlayerState])(mob: Mob): Mob = {

    def nextPositionAlong(position: V2[Double], direction: Direction, dt: Double) : V2[Double] =
      position + (dt * mob.speed / Tick.ticksPerSecond) *: direction.vector

    def isIllegal(position: V2[Double]): Boolean = {
      val wouldCollide = map.doesRectCollide(mob.template.appearance.collisionBox.translate(position))
      val wouldBeFarFromSpawn = (position - mob.spawn.position).lengthSq >= maxDistanceFromSpawnSq
      wouldCollide || wouldBeFarFromSpawn
    }

    def respawned: Mob = mob.copy(
      position = mob.spawn.position,
      nextPosition = mob.spawn.position,
      direction = Direction.none,
      attackTarget = None
    )

    def withRandomLegalDirection(position: V2[Double]): Mob = {
      val candidates = Direction.allMoving
        .map { dir => dir -> nextPositionAlong(position, dir, dt = 1) }
        .filter { case (_, next) => !isIllegal(next)  }
      random.shuffle(candidates).headOption.fold(respawned) {
        case (direction, nextPosition) =>
          mob.copy(position = position, nextPosition = nextPosition, direction = direction, attackTarget = None)
      }
    }

    // The client is already extrapolating the mob towards the next position,
    // so the next position should have already been set to a legal one at the previous tick.
    // Therefore we check if the position two ticks later is valid
    // and go ahead with the next position according to the current direction.
    val afterNextPosition = nextPositionAlong(mob.nextPosition, mob.direction, dt = 1)

    if (isIllegal(afterNextPosition)) {
      withRandomLegalDirection(mob.nextPosition)
    } else {
      val directionToTarget = attackTarget
        .map { target =>
          val direction = Direction.fromVector(target.collisionBoxCenter - mob.collisionBoxCenter)
          val nextPosition = nextPositionAlong(mob.nextPosition, direction, dt = 1)
          direction -> nextPosition
        }
        .filter { case (dir, next) => !isIllegal(next) && !isIllegal(nextPositionAlong(mob.nextPosition, dir, dt = 5)) }
      directionToTarget match {
        case Some((direction, nextPosition)) =>
          mob.copy(position = mob.nextPosition, nextPosition = nextPosition, direction = direction)
        case None if !mob.direction.isMoving || random.nextDouble() < 0.01 =>
          withRandomLegalDirection(mob.nextPosition)
        case None =>
          mob.copy(position = mob.nextPosition, nextPosition = afterNextPosition)
      }
    }
  }
}
