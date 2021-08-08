package mmo.server

import mmo.common.api.{Constants, Direction, LookDirection, PlayerCommand, PlayerConnected, PlayerDisappeared, PlayerDisconnected, PlayerEvent, PlayerId, PlayerPositionsChanged, Pong, SessionEstablished, Teleported}
import mmo.common.linear.V2

import akka.actor.typed.Behavior
import akka.actor.typed.scaladsl.Behaviors
import akka.stream.scaladsl.SourceQueueWithComplete
import java.util.UUID

final case class PlayerState(
  id: PlayerId,
  name: String,
  position: V2[Double],
  direction: Direction,
  lookDirection: LookDirection,
  queue: SourceQueueWithComplete[PlayerEvent],
  receivedAtNano: Long
)

object GameActor {
  sealed trait Message
  final case class Connected(id: PlayerId, queue: SourceQueueWithComplete[PlayerEvent]) extends Message
  final case class PlayerCommandReceived(id: PlayerId, command: PlayerCommand) extends Message
  final case class Disconnected(id: PlayerId) extends Message

  private object positionConstraints {
    // TODO this is flaky
    val maxAllowedDistanceSqFromPredicted: Double = 1.5

    // Allow a diagonal tile traversal and some more
    val maxAllowedDistanceSqFromLast: Double = 2 * (1.1 * 1.1)
  }
}

class GameActor(gameMap: ServerGameMap) {
  import GameActor._

  def start: Behavior[Message] = running(state = Map.empty)

  def running(state: Map[PlayerId, PlayerState]): Behavior[Message] =
    Behaviors.receiveMessage {

      case Connected(id, queue) =>
        val name = UUID.randomUUID().toString.take(6).toUpperCase
        val player = PlayerState(id, name, V2.zero, Direction.none, LookDirection.down, queue, System.nanoTime())
        broadcastPlayerConnection(player, state)
        broadcastPlayerPosition(player, state)

        val newState = state.updated(player.id, player)
        val playerNames = newState.map { case (id, player) => id -> player.name }.toSeq
        queue.offer(SessionEstablished(id, playerNames, gameMap.compactGameMap))
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
            val movedToObstacle = gameMap.gameMap.doesRectCollide(Constants.playerHitbox.translate(position))
            val movedFarFromLastPosition = (position - existing.position).lengthSq >= positionConstraints.maxAllowedDistanceSqFromLast
            val movedFarFromPredicted = (predictedPosition - position).lengthSq >= positionConstraints.maxAllowedDistanceSqFromPredicted
            val force = movedToObstacle || movedFarFromLastPosition || movedFarFromPredicted

            val enteredTeleport = if (force) {
              Option.empty[ServerGameMap.Teleport]
            } else {
              findTeleportAt(position, gameMap.teleports)
            }

            enteredTeleport match {
              case Some(teleport) =>
                val newPlayer = existing.copy(
                  position = teleport.targetPosition,
                  direction = Direction.none,
                  receivedAtNano = System.nanoTime()
                )
                val newState = state.updated(newPlayer.id, newPlayer)
                broadcastTeleport(newPlayer, newState)
                running(newState)

              case None =>
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
            }
          case None =>
            Behaviors.same
        }

      case PlayerCommandReceived(id, PlayerCommand.InitiateSession(_)) =>
        disconnectPlayer(id, state)

      case Disconnected(id) =>
        disconnectPlayer(id, state)
    }

  private def disconnectPlayer(id: PlayerId, state: Map[PlayerId, PlayerState]): Behavior[Message] = {
    val newState = state.removed(id)
    state.get(id).foreach { player =>
      player.queue.complete()
      broadcastEvent(PlayerDisconnected(id), newState)
    }
    running(newState)
  }

  private def broadcastPlayerPosition(player: PlayerState, state: Map[PlayerId, PlayerState], force: Boolean = false): Unit = {
    val event = PlayerPositionsChanged(Seq(playerStateToEvent(player, force)))
    broadcastEvent(event, state)
  }

  private def broadcastPlayerConnection(player: PlayerState, state: Map[PlayerId, PlayerState]): Unit = {
    val event = PlayerConnected(player.id, player.name)
    broadcastEvent(event, state - player.id)
  }

  private def broadcastTeleport(player: PlayerState, state: Map[PlayerId, PlayerState]): Unit =
    state.values.foreach { recipient =>
      val isOnTargetMap = true
      val isOnPreviousMap = false
      val event: List[PlayerEvent] = if (recipient.id == player.id) {
        val playersOnMap = state - player.id
        List(
          Teleported(player.position, gameMap.compactGameMap),
          PlayerPositionsChanged(playersOnMap.values.map(playerStateToEvent(_, force = true)).toSeq)
        )
      } else if (isOnTargetMap) {
        List(PlayerPositionsChanged(Seq(playerStateToEvent(player, force = true))))
      } else if (isOnPreviousMap) {
        List(PlayerDisappeared(player.id))
      } else {
        Nil
      }
      event.foreach(recipient.queue.offer)
    }

  private def broadcastEvent(event: PlayerEvent, state: Map[PlayerId, PlayerState]): Unit =
    state.values.foreach(_.queue.offer(event))

  private def playerStateToEvent(player: PlayerState, force: Boolean): PlayerPositionsChanged.Entry =
    PlayerPositionsChanged.Entry(player.id, player.position, player.direction, player.lookDirection, force)

  private def findTeleportAt(position: V2[Double], teleports: Seq[ServerGameMap.Teleport]): Option[ServerGameMap.Teleport] = {
    val hitboxCenter = position + Constants.playerHitboxCenter
    teleports.find(_.rect.contains(hitboxCenter))
  }
}
