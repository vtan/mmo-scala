package mmo.server

import mmo.common.api.{Direction, PlayerCommand, PlayerDisconnected, PlayerEvent, PlayerPositionsChanged, Pong, SessionEstablished}
import mmo.common.linear.V2

import akka.actor.typed.Behavior
import akka.actor.typed.scaladsl.Behaviors
import akka.stream.scaladsl.SourceQueueWithComplete
import java.util.UUID

final case class PlayerState(
  id: UUID,
  position: V2[Float],
  direction: Direction,
  queue: SourceQueueWithComplete[PlayerEvent],
  receivedAtNano: Long
)

object GameActor {
  sealed trait Message
  final case class Connected(id: UUID, queue: SourceQueueWithComplete[PlayerEvent]) extends Message
  final case class PlayerCommandReceived(id: UUID, command: PlayerCommand) extends Message
  final case class Disconnected(id: UUID) extends Message

  // TODO this doesn't work
  private val positionPredictionThreshold: Float = 1.0f

  def start: Behavior[Message] = running(state = Map.empty)

  def running(state: Map[UUID, PlayerState]): Behavior[Message] =
    Behaviors.receiveMessage {

      case Connected(id, queue) =>
        val player = PlayerState(id, V2.zero, Direction.none, queue, System.nanoTime())
        broadcastPlayerPosition(player, state)

        val newState = state.updated(player.id, player)
        queue.offer(SessionEstablished(id))
        queue.offer(PlayerPositionsChanged(
          newState.values.map(playerStateToEvent(_, force = true)).toSeq
        ))
        running(newState)

      case PlayerCommandReceived(id, PlayerCommand.Ping(clientTimeNano)) =>
        state.get(id).foreach { player =>
          player.queue.offer(Pong(clientTimeNano))
        }
        Behaviors.same

      case PlayerCommandReceived(id, PlayerCommand.Move(position, direction)) =>
        state.get(id) match {
          case Some(existing) =>
            val predictedPosition = {
              val timeElapsed = (System.nanoTime() - existing.receivedAtNano).toFloat * 1e-9f
              existing.position + timeElapsed *: existing.direction.vector
            }
            val isPositionInvalid = !(position.x >= 0 && position.y >= 0 && position.x < 32 && position.y < 32)
            val isPostitionFarFromPredicted = (predictedPosition - position).lengthSq >= positionPredictionThreshold
            val newPlayer = existing.copy(
              position = if (isPositionInvalid) {
                existing.position
              } else if (isPostitionFarFromPredicted) {
                predictedPosition
              } else {
                position
              },
              direction = direction,
              receivedAtNano = System.nanoTime()
            )
            val newState = state.updated(newPlayer.id, newPlayer)
            broadcastPlayerPosition(newPlayer, newState, force = isPositionInvalid || isPostitionFarFromPredicted)
            running(newState)
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

  private def broadcastPlayerPosition(player: PlayerState, state: Map[UUID, PlayerState], force: Boolean = false): Unit = {
    val event = PlayerPositionsChanged(Seq(playerStateToEvent(player, force)))
    broadcastEvent(event, state)
  }

  private def broadcastEvent(event: PlayerEvent, state: Map[UUID, PlayerState]): Unit =
    state.values.foreach(_.queue.offer(event))

  private def playerStateToEvent(player: PlayerState, force: Boolean): PlayerPositionsChanged.Entry =
    PlayerPositionsChanged.Entry(player.id, player.position, player.direction, force)
}
