package mmo.server.game

import mmo.common.api.{Constants, Direction, EntityAttacked, EntityDamaged, EntityPositionsChanged, Id, MobId, PlayerId}
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
      val attackTarget = mob.attackTarget.flatMap(state.players.get)
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

      val changedDirection = mob.direction != newMob.direction
      val enoughTicksPassed = state.tick - mob.lastBroadcastTick >= Tick.largeTickFrequency
      (newMob, changedDirection || enoughTicksPassed, attack)
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
    val candidates = state.players.values.collect(Function.unlift { player =>
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
      position + (dt * Constants.mobTilePerSecond / Tick.ticksPerSecond) *: direction.vector

    def isIllegal(position: V2[Double]): Boolean = {
      val wouldCollide = map.doesRectCollide(mob.template.appearance.collisionBox.translate(position))
      val wouldBeFarFromSpawn = (position - mob.spawn.position).lengthSq >= maxDistanceFromSpawnSq
      wouldCollide || wouldBeFarFromSpawn
    }

    def respawned: Mob = mob.copy(position = mob.spawn.position, direction = Direction.none, attackTarget = None)

    def withRandomLegalDirection(position: V2[Double]): Mob = {
      val candidates = Direction.allMoving.filter { dir =>
        !isIllegal(nextPositionAlong(position, dir, dt = 1)) &&
          !isIllegal(nextPositionAlong(position, dir, dt = 3))
      }
      random.shuffle(candidates).headOption.fold(respawned) { direction =>
        mob.copy(position = position, direction = direction, attackTarget = None)
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
    } else if (isIllegal(nextPosition2)) {
      withRandomLegalDirection(nextPosition)
    } else {
      val directionToTarget = attackTarget
        .map { target => Direction.fromVector(target.collisionBoxCenter - mob.collisionBoxCenter) }
        .filter { direction => !isIllegal(nextPositionAlong(nextPosition, direction, dt = 1)) }
      directionToTarget match {
        case Some(direction) =>
          mob.copy(position = nextPosition, direction = direction)
        case None if !mob.direction.isMoving || random.nextDouble() < 0.01 =>
          withRandomLegalDirection(nextPosition)
        case None =>
          mob.copy(position = nextPosition)
      }
    }
  }
}
