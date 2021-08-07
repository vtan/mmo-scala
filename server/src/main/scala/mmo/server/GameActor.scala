package mmo.server

import mmo.common.api.{CompactGameMap, Constants, Direction, LookDirection, PlayerCommand, PlayerDisconnected, PlayerEvent, PlayerPositionsChanged, Pong, SessionEstablished}
import mmo.common.linear.V2
import mmo.common.map.GameMap

import akka.actor.typed.Behavior
import akka.actor.typed.scaladsl.Behaviors
import akka.stream.scaladsl.SourceQueueWithComplete
import java.util.UUID

final case class PlayerState(
  id: UUID,
  position: V2[Double],
  direction: Direction,
  lookDirection: LookDirection,
  queue: SourceQueueWithComplete[PlayerEvent],
  receivedAtNano: Long
)

object GameActor {
  sealed trait Message
  final case class Connected(id: UUID, queue: SourceQueueWithComplete[PlayerEvent]) extends Message
  final case class PlayerCommandReceived(id: UUID, command: PlayerCommand) extends Message
  final case class Disconnected(id: UUID) extends Message

  private object positionConstraints {
    // TODO this is flaky
    val maxAllowedDistanceSqFromPredicted: Double = 1.5

    // Allow a diagonal tile traversal and some more
    val maxAllowedDistanceSqFromLast: Double = 2 * (1.1 * 1.1)
  }
}

class GameActor(gameMap: GameMap) {
  import GameActor._

  // TODO since we have this separately, we need the game map only for collision detection
  private val compactGameMap: CompactGameMap = CompactGameMap.from(gameMap)

  def start: Behavior[Message] = running(state = Map.empty)

  def running(state: Map[UUID, PlayerState]): Behavior[Message] =
    Behaviors.receiveMessage {

      case Connected(id, queue) =>
        val player = PlayerState(id, V2.zero, Direction.none, LookDirection.down, queue, System.nanoTime())
        broadcastPlayerPosition(player, state)

        val newState = state.updated(player.id, player)
        queue.offer(SessionEstablished(id, compactGameMap))
        queue.offer(PlayerPositionsChanged(
          newState.values.map(playerStateToEvent(_, force = true)).toSeq
        ))
        running(newState)

      case PlayerCommandReceived(id, PlayerCommand.Ping(clientTimeNano)) =>
        state.get(id).foreach { player =>
          player.queue.offer(Pong(clientTimeNano))
        }
        Behaviors.same

      case PlayerCommandReceived(id, PlayerCommand.Move(position, direction, lookDirection)) =>
        state.get(id) match {
          case Some(existing) =>
            val predictedPosition = {
              val timeElapsed = (System.nanoTime() - existing.receivedAtNano).toDouble * 1e-9
              existing.position + timeElapsed *: existing.direction.vector
            }
            val movedToObstacle = gameMap.doesRectCollide(Constants.playerHitbox.translate(position))
            val movedFarFromLastPosition = (position - existing.position).lengthSq >= positionConstraints.maxAllowedDistanceSqFromLast
            val movedFarFromPredicted = (predictedPosition - position).lengthSq >= positionConstraints.maxAllowedDistanceSqFromPredicted
            val force = movedToObstacle || movedFarFromLastPosition || movedFarFromPredicted
            val newPlayer = existing.copy(
              position = if (movedToObstacle || movedFarFromLastPosition) {
                existing.position
              } else if (movedFarFromPredicted) {
                predictedPosition
              } else {
                position
              },
              direction = if (movedToObstacle || movedFarFromLastPosition) {
                Direction.none
              } else {
                direction
              },
              lookDirection = lookDirection,
              receivedAtNano = System.nanoTime()
            )
            val newState = state.updated(newPlayer.id, newPlayer)
            broadcastPlayerPosition(newPlayer, newState, force = force)
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
    PlayerPositionsChanged.Entry(player.id, player.position, player.direction, player.lookDirection, force)
}
