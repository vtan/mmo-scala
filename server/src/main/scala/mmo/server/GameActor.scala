package mmo.server

import mmo.common.api.{Direction, PlayerCommand, PlayerDisconnected, PlayerEvent, PlayerPositionChanged, SessionEstablished}
import mmo.common.linear.V2

import akka.actor.typed.Behavior
import akka.actor.typed.scaladsl.Behaviors
import akka.stream.scaladsl.SourceQueueWithComplete
import java.util.UUID

final case class PlayerState(
  id: UUID,
  position: V2[Float],
  direction: Direction,
  queue: SourceQueueWithComplete[PlayerEvent]
)

object GameActor {
  sealed trait Message
  final case class Connected(id: UUID, queue: SourceQueueWithComplete[PlayerEvent]) extends Message
  final case class PlayerCommandReceived(id: UUID, command: PlayerCommand) extends Message
  final case class Disconnected(id: UUID) extends Message

  def start: Behavior[Message] = running(state = Map.empty)

  def running(state: Map[UUID, PlayerState]): Behavior[Message] =
    Behaviors.receiveMessage {

      case Connected(id, queue) =>
        val player = PlayerState(id, V2.zero, Direction.none, queue)
        broadcastPlayerPosition(player, state)

        val newState = state.updated(player.id, player)
        queue.offer(SessionEstablished(id))
        queue.offer(PlayerPositionChanged(
          newState.values.map(playerStateToEvent).toSeq
        ))
        running(newState)

      case PlayerCommandReceived(id, PlayerCommand.Move(position, direction)) =>
        state.get(id) match {
          case Some(player) =>
            if (position.x >= 0 && position.y >= 0 && position.x < 32 && position.y < 32) {
              val newPlayer = player.copy(position = position, direction = direction)
              val newState = state.updated(newPlayer.id, newPlayer)
              broadcastPlayerPosition(newPlayer, newState)
              running(newState)
            } else {
              // TODO this is a mess
              if (direction.isMoving) {
                player.queue.offer(PlayerPositionChanged(Seq(playerStateToEvent(player))))
                Behaviors.same
              } else {
                val newPlayer = player.copy(direction = direction)
                val newState = state.updated(newPlayer.id, newPlayer)
                broadcastPlayerPosition(newPlayer, newState)
                running(newState)
              }
            }
          case None =>
            Behaviors.same
        }

      case PlayerCommandReceived(id, PlayerCommand.InitiateSession(_)) =>
        disconnectPlayer(id, state)

      case Disconnected(id) =>
        disconnectPlayer(id, state)
    }

  private def disconnectPlayer(id: UUID, state: Map[UUID, PlayerState]): Behavior[Message] = {
    val newState = state.removed(id)
    state.get(id).foreach { player =>
      player.queue.complete()
      broadcastEvent(PlayerDisconnected(id), newState)
    }
    running(newState)
  }

  private def broadcastPlayerPosition(player: PlayerState, state: Map[UUID, PlayerState]): Unit = {
    val event = PlayerPositionChanged(Seq(playerStateToEvent(player)))
    broadcastEvent(event, state)
  }

  private def broadcastEvent(event: PlayerEvent, state: Map[UUID, PlayerState]): Unit =
    state.values.foreach(_.queue.offer(event))

  private def playerStateToEvent(player: PlayerState): PlayerPositionChanged.Entry =
    PlayerPositionChanged.Entry(player.id, player.position, player.direction)
}
