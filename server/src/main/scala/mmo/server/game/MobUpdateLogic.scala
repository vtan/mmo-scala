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

  def updateMobs(state: GameState): GameState =
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
}
