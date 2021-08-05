package mmo.common.api

final case class LookDirection(spriteIndex: Int) extends AnyVal

object LookDirection {
  val down = LookDirection(0)
  val up = LookDirection(1)
  val right = LookDirection(2)
  val left = LookDirection(3)
}


