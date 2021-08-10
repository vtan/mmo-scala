package mmo.server.server

import mmo.common.api._
import mmo.server.game.{GameLogic, GameState, MobTemplate, ServerGameMap}

import akka.actor.typed.Behavior
import akka.actor.typed.scaladsl.Behaviors
import akka.stream.scaladsl.SourceQueueWithComplete
import org.slf4j.LoggerFactory

object GameActor {
  sealed trait Message
  final case class Connected(id: PlayerId, queue: SourceQueueWithComplete[PlayerEvent]) extends Message
  final case class PlayerCommandReceived(id: PlayerId, command: PlayerCommand) extends Message
  final case class Disconnected(id: PlayerId) extends Message
}

class GameActor(
  maps: Map[Id[ServerGameMap], ServerGameMap],
  mapNames: Map[String, Id[ServerGameMap]],
  mobTemplates: Map[String, MobTemplate]
) {
  import GameActor._

  private val log = LoggerFactory.getLogger(getClass)

  private val logic = new GameLogic(maps, mapNames, mobTemplates)

  def start: Behavior[Message] = running(state = GameState.empty)

  def running(state: GameState): Behavior[Message] =
    Behaviors.receiveMessage {
      case Connected(playerId, queue) =>
        try {
          val newState = logic.playerConnected(playerId, queue)(state)
          running(newState)
        } catch {
          case ex: Throwable =>
            log.error(s"Error while accepting connection for player $playerId", ex)
            Behaviors.same
        }

      case PlayerCommandReceived(id, PlayerCommand.Ping(clientTimeNano)) =>
        state.players.get(id).foreach { player =>
          player.queue.offer(Pong(clientTimeNano))
        }
        Behaviors.same

      case PlayerCommandReceived(playerId, command) =>
        try {
          val newState = logic.playerCommandReceived(playerId, command)(state)
          running(newState)
        } catch {
          case ex: Throwable =>
            log.error(s"Error while handling command ${command.getClass.getSimpleName} from player $playerId", ex)
            running(logic.disconnectPlayer(playerId)(state))
        }

      case Disconnected(playerId) =>
        running(state.removePlayer(playerId))
    }
}
