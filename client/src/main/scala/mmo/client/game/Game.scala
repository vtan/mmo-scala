package mmo.client.game

import mmo.client.graphics.{FpsCounter, GlfwEvent, GlfwUtil, KeyboardEvent, TileAtlas}
import mmo.client.network.{CommandSender, EventReceiver}
import mmo.common.api.{Direction, PlayerCommand, PlayerDisconnected, PlayerPositionChanged}
import mmo.common.linear.V2

import java.util.UUID
import java.util.concurrent.atomic.AtomicReference
import org.lwjgl.glfw.GLFW._
import org.lwjgl.nanovg.NanoVG._
import org.lwjgl.nanovg.NanoVGGL3._
import org.lwjgl.opengl.GL11C._

class Game(
  window: Long,
  playerId: UUID,
  glfwEventsRef: AtomicReference[List[GlfwEvent]],
  eventReceiver: EventReceiver,
  commandSender: CommandSender
) {
  private val nvg: Long = nvgCreate(0)
  private val (screenSize: V2[Float], pixelRatio: Float) = GlfwUtil.getWindowGeometry(window)

  private val tileAtlas: TileAtlas = TileAtlas.loadFromFile(nvg, "assets/tileset.png")
  private val font: Int = nvgCreateFont(nvg, "Roboto", "assets/roboto/Roboto-Regular.ttf")
  nvgFontFaceId(nvg, font)

  private val scaleFactor: Int = 3
  private val tilePerSecond: Float = 4.0f

  private val playerStates = scala.collection.mutable.Map.empty[UUID, PlayerState]

  def run(): Unit = {
    val fpsCounter = new FpsCounter
    var now: Double = glfwGetTime()
    var lastFrameTime: Double = now

    while (!glfwWindowShouldClose(window)) {
      val events = glfwEventsRef.getAndSet(Nil).reverse
      update(events, now.toFloat, (now - lastFrameTime).toFloat)

      render()
      glfwSwapBuffers(window)
      glfwPollEvents()

      lastFrameTime = now
      now = glfwGetTime()
      fpsCounter.endOfFrame(window, now)
    }
  }

  private def update(events: List[GlfwEvent], now: Float, dt: Float): Unit = {
    events.foreach {
      case KeyboardEvent(key, KeyboardEvent.Press) =>
        val direction = key match {
          case GLFW_KEY_RIGHT => Direction.right
          case GLFW_KEY_LEFT => Direction.left
          case GLFW_KEY_UP => Direction.up
          case GLFW_KEY_DOWN => Direction.down
          case _ => Direction.none
        }
        if (direction.isMoving) {
          playerStates.updateWith(playerId)(_.map { state =>
            commandSender.offer(PlayerCommand.Move(state.position, direction))
            state.copy(direction = direction)
          })
        }

      case KeyboardEvent(key, KeyboardEvent.Release) =>
        key match {
          case GLFW_KEY_RIGHT | GLFW_KEY_LEFT | GLFW_KEY_UP | GLFW_KEY_DOWN =>
            playerStates.updateWith(playerId)(_.map { state =>
              val direction = Direction.none
              commandSender.offer(PlayerCommand.Move(state.position, direction))
              state.copy(direction = direction)
            })
          case _ => ()
        }

      case _ => ()
    }

    playerStates.mapValuesInPlace { (id, state) =>
      if (id == playerId) {
        if (state.direction.isMoving) {
          val normal = state.direction.vector
          val positionChange = (dt * tilePerSecond) *: normal
          val newPosition = state.position + positionChange
          val hasCrossedTile = newPosition.map(_.toInt) - state.previousPosition.map(_.toInt) != V2.zero[Int]
          if (hasCrossedTile) {
            commandSender.offer(PlayerCommand.Move(newPosition, state.direction))
          }
          state.copy(
            position = newPosition,
            previousPosition = state.position
          )
        } else {
          state
        }

      } else {
        if (state.direction.isMoving) {
          val interpolationStartPeriod = 0.5f
          if (now - state.receivedAt <= interpolationStartPeriod) {
            // Interpolating to predicted position in the near future to avoid jumping on a new update from server
            val t = (now - state.receivedAt) / interpolationStartPeriod
            val target = state.lastPositionFromServer + (interpolationStartPeriod * tilePerSecond) *: state.direction.vector
            val position = ((1 - t) *: state.smoothedPositionAtLastServerUpdate) + (t *: target)
            state.copy(
              position = position,
              previousPosition = state.smoothedPositionAtLastServerUpdate
            )
          } else {
            val interpolatedMovement = ((now - state.receivedAt) * tilePerSecond) *: state.direction.vector
            val position = state.lastPositionFromServer + interpolatedMovement
            state.copy(
              position = position,
              previousPosition = state.lastPositionFromServer
            )
          }
        } else {
          state
        }
      }
    }

    var nextPlayerEvent = eventReceiver.poll()
    while (nextPlayerEvent != null) {
      nextPlayerEvent match {
        case PlayerPositionChanged(positions) =>
          positions.foreach { entry =>
            playerStates.updateWith(entry.id) {
              case Some(old) =>
                if (entry.direction.isMoving) {
                  if (entry.id == playerId) {
                    Some(old.copy(lastPositionFromServer = entry.position, receivedAt = now))
                  } else {
                    Some(old.copy(lastPositionFromServer = entry.position, smoothedPositionAtLastServerUpdate = old.position, direction = entry.direction, receivedAt = now))
                  }
                } else {
                  Some(old.copy(position = entry.position, previousPosition = old.previousPosition, lastPositionFromServer = entry.position, smoothedPositionAtLastServerUpdate = old.position, direction = entry.direction, receivedAt = now))
                }
              case None =>
                Some(PlayerState(position = entry.position, previousPosition = entry.position, lastPositionFromServer = entry.position, smoothedPositionAtLastServerUpdate = entry.position, direction = entry.direction, receivedAt = now))
            }
          }
        case PlayerDisconnected(id) =>
          playerStates.remove(id)
      }
      nextPlayerEvent = eventReceiver.poll()
    }
  }

  private def render(): Unit = {
    glClearColor(0.0f, 0.0f, 0.0f, 0.0f)
    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT | GL_STENCIL_BUFFER_BIT)

    nvgBeginFrame(nvg, screenSize.x, screenSize.y, pixelRatio)

    val (grass, water) = (V2(0, 2), V2(1, 2))
    (0 to 32).foreach { x =>
      (0 to 32).foreach { y =>
        val tile = if ((x - 12) * (x - 12) + (y - 6) * (y - 6) <= 64) water else grass
        tileAtlas.render(
          nvg,
          screenPosition = ((scaleFactor * TileAtlas.tileSize) *: V2(x, y)).map(_.toFloat),
          tilePosition = tile,
          tileCount = V2(1, 1),
          scaleFactor = scaleFactor
        )
      }
    }

    playerStates.foreach {
      case (_, player) =>
        tileAtlas.render(
          nvg,
          screenPosition = (scaleFactor * TileAtlas.tileSize).toFloat *: (player.position - V2(0, 1)),
          tilePosition = V2(player.directionTileIndex, 0),
          tileCount = V2(1, 2),
          scaleFactor = scaleFactor
        )
    }

    nvgStrokeColor(nvg, GlfwUtil.color(0, 0, 0))
    playerStates.foreach {
      case (_, player) =>
        val V2(x, y) = (scaleFactor * TileAtlas.tileSize).toFloat *: player.lastPositionFromServer
        val size = (scaleFactor * TileAtlas.tileSize).toFloat
        nvgBeginPath(nvg)
        nvgRect(nvg, x, y, size, size)
        nvgStroke(nvg)
    }

    nvgEndFrame(nvg)
  }
}
