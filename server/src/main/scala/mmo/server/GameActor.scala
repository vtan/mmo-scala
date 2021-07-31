package mmo.server

import akka.actor.typed.Behavior
import akka.actor.typed.scaladsl.Behaviors
import akka.stream.scaladsl.SourceQueue
import java.util.UUID
import mmo.common.{PlayerCommand, PlayerDisconnected, PlayerEvent, PlayerPositionChanged}

final case class PlayerState(
  id: UUID,
  x: Int,
  y: Int,
  queue: SourceQueue[PlayerEvent]
)

object GameActor {
  sealed trait Message
  final case class Connected(id: UUID, queue: SourceQueue[PlayerEvent]) extends Message
  final case class PlayerCommandReceived(id: UUID, command: PlayerCommand) extends Message
  final case class Disconnected(id: UUID) extends Message

  def start: Behavior[Message] = running(state = Map.empty)

  def running(state: Map[UUID, PlayerState]): Behavior[Message] =
    Behaviors.receiveMessage {

      case Connected(id, queue) =>
        val player = PlayerState(id, 0, 0, queue)
        broadcastPlayerPosition(player, state)

        val newState = state.updated(player.id, player)
        queue.offer(PlayerPositionChanged(
          newState.values.map(playerStateToEvent).toSeq
        ))
        running(newState)

      case PlayerCommandReceived(id, command) =>
        state.get(id) match {
          case Some(player) =>
            val (dx, dy) = command match {
              case PlayerCommand.Move(dx, dy) => (dx, dy)
            }
            val x = player.x + dx
            val y = player.y + dy
            if (x >= 0 && y >= 0 && x < 32 && y < 32) {
              val newPlayer = player.copy(x = x, y = y)
              val newState = state.updated(newPlayer.id, newPlayer)
              broadcastPlayerPosition(newPlayer, newState)
              running(newState)
            } else {
              player.queue.offer(PlayerPositionChanged(Seq(playerStateToEvent(player))))
              Behaviors.same
            }
          case None =>
            Behaviors.same
        }

      case Disconnected(id) =>
        val newState = state.removed(id)
        broadcastEvent(PlayerDisconnected(id), newState)
        running(newState)
    }

  private def broadcastPlayerPosition(player: PlayerState, state: Map[UUID, PlayerState]): Unit = {
    val event = PlayerPositionChanged(Seq(playerStateToEvent(player)))
    broadcastEvent(event, state)
  }

  private def broadcastEvent(event: PlayerEvent, state: Map[UUID, PlayerState]): Unit =
    state.values.foreach(_.queue.offer(event))

  private def playerStateToEvent(player: PlayerState): PlayerPositionChanged.Entry =
    PlayerPositionChanged.Entry(player.id, player.x, player.y)
}
