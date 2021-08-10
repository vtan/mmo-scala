package mmo.common.api

sealed trait EntityId

final case class PlayerId(asLong: Long) extends EntityId

final case class MobId(asLong: Long) extends EntityId
