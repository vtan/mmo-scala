package mmo.client.graphics

sealed trait GlfwEvent

final case class KeyboardEvent(key: Int, action: KeyboardEvent.Action) extends GlfwEvent

object KeyboardEvent {
  sealed trait Action
  case object Press extends Action
  case object Release extends Action
  case object Repeat extends Action
}
