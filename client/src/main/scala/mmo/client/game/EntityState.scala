package mmo.client.game

import mmo.common.api.{Constants, Direction, EntityPositionsChanged, LookDirection, EntityAppeared, MobId, PlayerId}
import mmo.common.linear.V2

final case class EntityState(
  position: V2[Double],
  lastPositionFromServer: V2[Double],
  interpolationSource: V2[Double],
  interpolationTarget: V2[Double],
  direction: Direction,
  speedTilePerSecond: Double,
  lookDirection: LookDirection,
  directionLastChangedAt: Double,
  lastServerEventAt: Double,
  attackAnimationStarted: Double,
  dyingAnimationStarted: Option[Double],
  maxHitPoints: Int,
  hitPoints: Int
) {

  def applyPositionChange(update: EntityPositionsChanged.Entry, now: Double, calculateInterpolation: Boolean): EntityState =
    copy(
      position = if (calculateInterpolation) update.position else this.position,
      lastPositionFromServer = update.position,
      direction = update.direction,
      lookDirection = update.lookDirection,
      directionLastChangedAt = if (this.lookDirection == update.lookDirection) this.directionLastChangedAt else now,
      lastServerEventAt = now,

      interpolationSource = if (calculateInterpolation) {
        this.position
      } else {
        V2.zero
      },
      interpolationTarget = if (calculateInterpolation) {
        update.position + (EntityState.interpolationPeriod * speedTilePerSecond) *: update.direction.vector
      } else {
        V2.zero
      }
    )

  def spriteOffsetAt(time: Double): Int =
    if (dyingAnimationStarted.isDefined) {
      lookDirection.spriteIndex
    } else if (time < attackAnimationStarted + Constants.playerAttackLength) {
      val t = (time - attackAnimationStarted) / Constants.playerAttackLength
      val offset = (t * 5).toInt match {
        case 0 => 0
        case 1 => 1
        case 2 => 2
        case 3 => 1
        case _ => 0
      }
      lookDirection.spriteIndex + offset
    } else if (direction.isMoving) {
      val offset = (((time - directionLastChangedAt) / 0.15f) % 4).toInt match {
        case 0 => 1
        case 1 => 0
        case 2 => 2
        case 3 => 0
      }
      lookDirection.spriteIndex + offset
    } else {
      lookDirection.spriteIndex
    }

  def isInterpolatingAt(t: Double): Boolean =
    t - lastServerEventAt <= EntityState.interpolationPeriod
}

object EntityState {
  val interpolationPeriod = 0.1
  val dyingAnimationPeriod = 0.15
  val dyingAnimationLength = 8 * dyingAnimationPeriod

  def newAt(update: EntityPositionsChanged.Entry, now: Double): EntityState =
    EntityState(
      position = update.position,
      lastPositionFromServer = update.position,
      interpolationSource = update.position,
      interpolationTarget = update.position,
      direction = update.direction,
      speedTilePerSecond = update.entityId match {
        case _: PlayerId => Constants.playerTilePerSecond
        case _: MobId => Constants.mobTilePerSecond
      },
      lookDirection = update.lookDirection,
      directionLastChangedAt = now,
      lastServerEventAt = now,
      attackAnimationStarted = Double.NegativeInfinity,
      dyingAnimationStarted = None,
      maxHitPoints = 1,
      hitPoints = 0
    )

  def newAt(event: EntityAppeared, now: Double): EntityState =
    EntityState(
      position = event.position,
      lastPositionFromServer = event.position,
      interpolationSource = event.position,
      interpolationTarget = event.position,
      direction = event.direction,
      speedTilePerSecond = event.id match {
        case _: PlayerId => Constants.playerTilePerSecond
        case _: MobId => Constants.mobTilePerSecond
      },
      lookDirection = event.lookDirection,
      directionLastChangedAt = now,
      lastServerEventAt = now,
      attackAnimationStarted = Double.NegativeInfinity,
      dyingAnimationStarted = None,
      maxHitPoints = event.maxHitPoints,
      hitPoints = event.hitPoints
    )
}
