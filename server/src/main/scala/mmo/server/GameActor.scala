package mmo.server

import mmo.common.api.{Constants, Direction, Id, LookDirection, MovementAcked, OtherPlayerConnected, OtherPlayerDisappeared, OtherPlayerDisconnected, PlayerCommand, PlayerEvent, PlayerId, PlayerPositionsChanged, Pong, SessionEstablished, Teleported}
import mmo.common.linear.V2

import akka.actor.typed.Behavior
import akka.actor.typed.scaladsl.Behaviors
import akka.stream.scaladsl.SourceQueueWithComplete
import java.util.UUID

final case class PlayerState(
  id: PlayerId,
  name: String,
  mapId: Id[ServerGameMap],
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

class GameActor(
  maps: Map[Id[ServerGameMap], ServerGameMap],
  mapNames: Map[String, Id[ServerGameMap]]
) {
  import GameActor._

  def start: Behavior[Message] = running(state = Map.empty)

  def running(state: Map[PlayerId, PlayerState]): Behavior[Message] =
    Behaviors.receiveMessage {
      // TODO: handle exceptions by disconnecting the player whose command triggered the error

      case Connected(playerId, queue) =>
        val name = UUID.randomUUID().toString.take(6).toUpperCase
        val (mapId, map) = maps.minBy(_._1.asLong)
        val player = PlayerState(playerId, name, mapId, V2(2, 1), Direction.none, LookDirection.down, queue, System.nanoTime())

        val newState = state.updated(player.id, player)
        val playerNames = newState.map { case (id, player) => id -> player.name }.toSeq

        queue.offer(SessionEstablished(playerId, playerNames, map.compactGameMap))
        broadcastPlayerConnection(player, state)
        broadcastPlayerPosition(player, state, ack = false)
        queue.offer(PlayerPositionsChanged(
          (newState - playerId).values.filter(_.mapId == mapId).map(playerStateToEvent).toSeq
        ))
        running(newState)

      case PlayerCommandReceived(id, PlayerCommand.Ping(clientTimeNano)) =>
        state.get(id).foreach { player =>
          player.queue.offer(Pong(clientTimeNano))
        }
        Behaviors.same

      case PlayerCommandReceived(id, requested: PlayerCommand.Move) =>
        state.get(id) match {
          case Some(existing) =>
            val predictedPosition = {
              val timeElapsed = (System.nanoTime() - existing.receivedAtNano).toDouble * 1e-9
              existing.position + timeElapsed *: existing.direction.vector
            }
            val map = maps(existing.mapId)
            val movedToObstacle = map.gameMap.doesRectCollide(Constants.playerHitbox.translate(requested.position))
            val movedFarFromLastPosition = (requested.position - existing.position).lengthSq >= positionConstraints.maxAllowedDistanceSqFromLast
            val movedFarFromPredicted = (predictedPosition - requested.position).lengthSq >= positionConstraints.maxAllowedDistanceSqFromPredicted
            val invalid = movedToObstacle || movedFarFromLastPosition || movedFarFromPredicted

            val enteredTeleport = if (!invalid) {
              findTeleportAt(existing.position, requested.position, map.teleports)
            } else {
              None
            }

            enteredTeleport match {
              case Some((teleport, targetMap)) =>
                val newPlayer = existing.copy(
                  mapId = targetMap.id,
                  position = teleport.targetPosition,
                  direction = Direction.none,
                  receivedAtNano = System.nanoTime()
                )
                val newState = state.updated(newPlayer.id, newPlayer)
                broadcastTeleport(newPlayer, existing.mapId, targetMap, newState)
                running(newState)

              case None =>
                val newPlayer = if (invalid) {
                  existing.copy(
                    position = if (movedFarFromPredicted) predictedPosition else existing.position,
                    direction = if (movedToObstacle) Direction.none else requested.direction,
                    lookDirection = requested.lookDirection,
                    receivedAtNano = System.nanoTime()
                  )
                } else {
                  existing.copy(
                    position = requested.position,
                    direction = requested.direction,
                    lookDirection = requested.lookDirection,
                    receivedAtNano = System.nanoTime()
                  )
                }
                val newState = state.updated(newPlayer.id, newPlayer)
                broadcastPlayerPosition(newPlayer, newState, ack = !invalid)
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
      broadcastEvent(OtherPlayerDisconnected(id), newState)
    }
    running(newState)
  }

  private def broadcastPlayerPosition(player: PlayerState, state: Map[PlayerId, PlayerState], ack: Boolean): Unit = {
    val toOthers = PlayerPositionsChanged(Seq(playerStateToEvent(player)))
    val toPlayer = if (ack) MovementAcked(player.position) else toOthers
    player.queue.offer(toPlayer)
    broadcastEvent(toOthers, (state - player.id).filter(_._2.mapId == player.mapId))
  }

  private def broadcastPlayerConnection(player: PlayerState, state: Map[PlayerId, PlayerState]): Unit = {
    val event = OtherPlayerConnected(player.id, player.name)
    broadcastEvent(event, state - player.id)
  }

  private def broadcastTeleport(player: PlayerState, previousMapId: Id[ServerGameMap], newMap: ServerGameMap, state: Map[PlayerId, PlayerState]): Unit =
    state.values.foreach { recipient =>
      val isOnTargetMap = recipient.mapId == newMap.id
      val isOnPreviousMap = recipient.mapId == previousMapId

      val event: List[PlayerEvent] = if (recipient.id == player.id) {
        val playersOnMap = state.filter(_._2.mapId == newMap.id)
        List(
          Teleported(newMap.compactGameMap),
          PlayerPositionsChanged(playersOnMap.values.map(playerStateToEvent).toSeq)
        )
      } else if (isOnTargetMap) {
        List(PlayerPositionsChanged(Seq(playerStateToEvent(player))))
      } else if (isOnPreviousMap) {
        List(OtherPlayerDisappeared(player.id))
      } else {
        Nil
      }

      event.foreach(recipient.queue.offer)
    }

  private def broadcastEvent(event: PlayerEvent, state: Map[PlayerId, PlayerState]): Unit =
    state.values.foreach(_.queue.offer(event))

  private def playerStateToEvent(player: PlayerState): PlayerPositionsChanged.Entry =
    PlayerPositionsChanged.Entry(player.id, player.position, player.direction, player.lookDirection)

  private def findTeleportAt(
    oldPosition: V2[Double],
    newPosition: V2[Double],
    teleports: Seq[ServerGameMap.Teleport]
  ): Option[(ServerGameMap.Teleport, ServerGameMap)] = {
    val oldHitboxCenter = oldPosition + Constants.playerHitboxCenter
    val newHitboxCenter = newPosition + Constants.playerHitboxCenter
    for {
      teleport <- teleports.find(tp => tp.rect.contains(newHitboxCenter) && !tp.rect.contains(oldHitboxCenter))
      mapId <- mapNames.get(teleport.targetMapName)
      map <- maps.get(mapId)
    } yield (teleport, map)
  }
}
