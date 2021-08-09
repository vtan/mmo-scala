package mmo.client.game

import mmo.common.api.Direction

import org.lwjgl.glfw.GLFW._

object MovementKeyBits {
  val left: Int = 1 << 0
  val right: Int = 1 << 1
  val up: Int = 1 << 2
  val down: Int = 1 << 3

  object Bit {
    def unapply(keycode: Int): Option[Int] =
      keycode match {
        case GLFW_KEY_LEFT => Some(MovementKeyBits.left)
        case GLFW_KEY_RIGHT => Some(MovementKeyBits.right)
        case GLFW_KEY_UP => Some(MovementKeyBits.up)
        case GLFW_KEY_DOWN => Some(MovementKeyBits.down)
        case _ => None
      }
  }

  val directions: Array[Direction] = Array(
    Direction.none,
    Direction.left,
    Direction.right, Direction.none,
    Direction.up, Direction.leftUp, Direction.rightUp, Direction.none,
    Direction.down, Direction.leftDown, Direction.rightDown, Direction.none,
    Direction.none, Direction.none, Direction.none, Direction.none
  )
}
