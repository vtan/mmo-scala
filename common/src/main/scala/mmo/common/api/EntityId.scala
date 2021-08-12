package mmo.common.api

import java.util.concurrent.atomic.AtomicLong

sealed trait EntityId

final case class PlayerId(asLong: Long) extends EntityId

final case class MobId(asLong: Long) extends EntityId

object PlayerId {
  private val nextPlayerId: AtomicLong = new AtomicLong(0)

  def nextId(): PlayerId = PlayerId(nextPlayerId.getAndIncrement())
}

object MobId {
  private val nextMobId: AtomicLong = new AtomicLong(0)

  def nextId(): MobId = MobId(nextMobId.getAndIncrement())
}
