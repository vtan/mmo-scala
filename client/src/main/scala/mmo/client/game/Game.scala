package mmo.client.game

import mmo.client.common.V2
import mmo.client.graphics.{FpsCounter, GlfwEvent, GlfwUtil, KeyboardEvent, TileAtlas}
import mmo.client.network.{CommandSender, EventReceiver}
import mmo.common.{PlayerCommand, PlayerDisconnected, PlayerPositionChanged}

import java.util.UUID
import java.util.concurrent.atomic.AtomicReference
import org.lwjgl.glfw.GLFW._
import org.lwjgl.nanovg.NanoVG._
import org.lwjgl.nanovg.NanoVGGL3._
import org.lwjgl.opengl.GL11C._

class Game(
  window: Long,
  glfwEventsRef: AtomicReference[List[GlfwEvent]],
  eventReceiver: EventReceiver,
  commandSender: CommandSender
) {
  private val nvg: Long = nvgCreate(NVG_ANTIALIAS)
  private val (screenSize: V2[Float], pixelRatio: Float) = GlfwUtil.getWindowGeometry(window)

  private val tileAtlas: TileAtlas = TileAtlas.loadFromFile(nvg, "assets/tileset.png")
  private val font: Int = nvgCreateFont(nvg, "Roboto", "assets/roboto/Roboto-Regular.ttf")
  nvgFontFaceId(nvg, font)

  private val scaleFactor: Int = 3

  private val playerStates = scala.collection.mutable.Map.empty[UUID, PlayerState]

  def run(): Unit = {
    val fpsCounter = new FpsCounter

    while (!glfwWindowShouldClose(window)) {
      val events = glfwEventsRef.getAndSet(Nil).reverse
      update(events)

      render()
      glfwSwapBuffers(window)
      glfwPollEvents()

      val now = glfwGetTime()
      fpsCounter.endOfFrame(window, now)
    }
  }

  private def update(events: List[GlfwEvent]): Unit = {
    events.foreach {
      case KeyboardEvent(key, KeyboardEvent.Press | KeyboardEvent.Repeat) =>
        val direction = key match {
          case GLFW_KEY_RIGHT => Some((1, 0))
          case GLFW_KEY_LEFT => Some((-1, 0))
          case GLFW_KEY_UP => Some((0, -1))
          case GLFW_KEY_DOWN => Some((0, 1))
          case _ => None
        }
        direction.foreach { case (dx, dy) =>
          val command = PlayerCommand.Move(dx, dy)
          commandSender.offer(command)
        }
      case _ => ()
    }

    var nextPlayerEvent = eventReceiver.poll()
    while (nextPlayerEvent != null) {
      nextPlayerEvent match {
        case PlayerPositionChanged(positions) =>
          positions.foreach { entry =>
            val position = V2(entry.x, entry.y)
            playerStates.updateWith(entry.id) {
              case Some(old) => Some(PlayerState(position = position, previousPosition = old.position))
              case None => Some(PlayerState(position = position, previousPosition = position))
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
          screenPosition = (scaleFactor * TileAtlas.tileSize) *: V2(x, y),
          tilePosition = tile,
          tileCount = V2(1, 1),
          scaleFactor = scaleFactor
        )

      }
    }

    playerStates.foreach {
      case (id, player) =>
        tileAtlas.render(
          nvg,
          screenPosition = (scaleFactor * TileAtlas.tileSize) *: (player.position - V2(0, 1)),
          tilePosition = V2(player.directionIndex, 0),
          tileCount = V2(1, 2),
          scaleFactor = scaleFactor
        )
    }

    nvgEndFrame(nvg)
  }
}
