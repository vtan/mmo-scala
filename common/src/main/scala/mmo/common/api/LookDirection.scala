package mmo.common.api

final case class LookDirection(spriteIndex: Int) extends AnyVal

object LookDirection {
  val down = LookDirection(0)
  val up = LookDirection(3)
  val right = LookDirection(6)
  val left = LookDirection(9)
}


