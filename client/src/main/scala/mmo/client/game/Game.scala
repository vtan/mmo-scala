package mmo.client.game

import mmo.client.graphics.{FpsCounter, GlfwEvent, GlfwUtil, KeyboardEvent, TileAtlas}
import mmo.client.network.{CommandSender, EventReceiver}
import mmo.common.api.{Constants, Direction, GameMap, PlayerCommand, PlayerDisconnected, PlayerPositionsChanged, Pong, SessionEstablished}
import mmo.common.linear.{Rect, V2}

import java.util.UUID
import java.util.concurrent.atomic.AtomicReference
import org.lwjgl.glfw.GLFW._
import org.lwjgl.nanovg.NanoVG._
import org.lwjgl.nanovg.NanoVGGL3._
import org.lwjgl.opengl.GL11C._

class Game(
  window: Long,
  playerId: UUID,
  gameMap: GameMap,
  glfwEventsRef: AtomicReference[List[GlfwEvent]],
  eventReceiver: EventReceiver,
  commandSender: CommandSender
) {
  private val nvg: Long = nvgCreate(0)
  private val (windowSize: V2[Float], pixelRatio: Float) = GlfwUtil.getWindowGeometry(window)
  private val windowGeometry = WindowGeometry(
    windowSize = windowSize,
    scaleFactor = 3
  )

  private val tileAtlas: TileAtlas = TileAtlas.loadFromFile(nvg, "assets/tileset.png")
  private val font: Int = nvgCreateFont(nvg, "Roboto", "assets/roboto/Roboto-Regular.ttf")
  nvgFontFaceId(nvg, font)

  object MovementKeyBits {
    val left: Int = 1 << 0
    val right: Int = 1 << 1
    val up: Int = 1 << 2
    val down: Int = 1 << 3

    val directions: Array[Direction] = Array(
      Direction.none,
      Direction.left,
      Direction.right, Direction.none,
      Direction.up, Direction.leftUp, Direction.rightUp, Direction.none,
      Direction.down, Direction.leftDown, Direction.rightDown, Direction.none,
      Direction.none, Direction.none, Direction.none, Direction.none
    )
  }
  private var movementKeyBits: Int = 0

  private var lastPingSent: Float = 0.0f
  private var lastPingRtt: String = ""
  private val playerStates = scala.collection.mutable.Map.empty[UUID, PlayerState]
  private var debugShowHitbox: Boolean = false

  def run(): Unit = {
    val fpsCounter = new FpsCounter
    var now: Double = glfwGetTime()
    var lastFrameTime: Double = now

    while (!glfwWindowShouldClose(window)) {
      val events = glfwEventsRef.getAndSet(Nil).reverse
      update(events, now.toFloat, (now - lastFrameTime).toFloat)

      render(now.toFloat)
      glfwSwapBuffers(window)
      glfwPollEvents()

      lastFrameTime = now
      now = glfwGetTime()
      fpsCounter.endOfFrame(window, now)
    }
  }

  private def update(events: List[GlfwEvent], now: Float, dt: Float): Unit = {
    if (now - lastPingSent >= 1.0f) {
      commandSender.offer(PlayerCommand.Ping(System.nanoTime()))
      lastPingSent = now
      lastPingRtt = s"RTT: ${(eventReceiver.lastPingNanos.get().toFloat * 1e-6f).toInt} ms"
    }

    val oldMovementKeyBits = movementKeyBits

    object MovementKeyBit {
      def unapply(keycode: Int): Option[Int] =
        keycode match {
          case GLFW_KEY_LEFT => Some(MovementKeyBits.left)
          case GLFW_KEY_RIGHT => Some(MovementKeyBits.right)
          case GLFW_KEY_UP => Some(MovementKeyBits.up)
          case GLFW_KEY_DOWN => Some(MovementKeyBits.down)
          case _ => None
        }
    }
    events.foreach {
      case KeyboardEvent(MovementKeyBit(bit), KeyboardEvent.Press) =>
        movementKeyBits |= bit

      case KeyboardEvent(MovementKeyBit(bit), KeyboardEvent.Release) =>
        movementKeyBits &= ~bit

      case KeyboardEvent(GLFW_KEY_F2, KeyboardEvent.Release) =>
        debugShowHitbox = !debugShowHitbox

      case _ => ()
    }

    if (movementKeyBits != oldMovementKeyBits) {
      val newDirection = MovementKeyBits.directions(movementKeyBits)
      playerStates.updateWith(playerId)(_.map { state =>
        val lookDirection = if (newDirection.isMoving) {
          newDirection.lookDirection
        } else {
          state.lookDirection
        }
        commandSender.offer(PlayerCommand.Move(state.position, newDirection, lookDirection))
        state.copy(direction = newDirection, lookDirection = lookDirection)
      })
    }

    playerStates.mapValuesInPlace { (id, state) =>
      if (id == playerId) {
        if (state.direction.isMoving) {
          val normal = state.direction.vector
          val positionChange = (dt * 1.0f * Constants.playerTilePerSecond) *: normal
          val newPosition = state.position + positionChange
          val hitbox = Constants.playerHitbox.translate(newPosition)
          if (gameMap.isRectWalkable(hitbox)) {
            val hasCrossedTile = newPosition.map(_.floor.toInt) - state.position.map(_.floor.toInt) != V2.zero[Int]
            if (hasCrossedTile) {
              commandSender.offer(PlayerCommand.Move(newPosition, state.direction, state.lookDirection))
            }
            state.copy(position = newPosition)
          } else {
            commandSender.offer(PlayerCommand.Move(state.position, Direction.none, state.lookDirection))
            state.copy(direction = Direction.none)
          }
        } else {
          state
        }
      } else {
        if (state.direction.isMoving) {
          val interpolationStartPeriod = 0.5f
          if (now - state.receivedAt <= interpolationStartPeriod) {
            // Interpolating to predicted position in the near future to avoid jumping on a new update from server
            val t = (now - state.receivedAt) / interpolationStartPeriod
            val target = state.lastPositionFromServer + (interpolationStartPeriod * Constants.playerTilePerSecond) *: state.direction.vector
            val position = ((1 - t) *: state.smoothedPositionAtLastServerUpdate) + (t *: target)
            state.copy(position = position)
          } else {
            val interpolatedMovement = ((now - state.receivedAt) * Constants.playerTilePerSecond) *: state.direction.vector
            val position = state.lastPositionFromServer + interpolatedMovement
            state.copy(position = position)
          }
        } else {
          state
        }
      }
    }

    var nextPlayerEvent = eventReceiver.poll()
    while (nextPlayerEvent != null) {
      nextPlayerEvent match {
        case PlayerPositionsChanged(positions) =>
          positions.foreach { update =>
            playerStates.updateWith(update.id) {
              case Some(old) =>
                if (update.id == playerId) {
                  if (update.force) {
                    Some(old.copy(
                      lastPositionFromServer = update.position,
                      direction = update.direction,
                      lookDirection = update.lookDirection,
                      receivedAt = now,
                      directionLastChangedAt = if (old.lookDirection == update.lookDirection) old.directionLastChangedAt else now
                    ))
                  } else {
                    Some(old.copy(
                      lastPositionFromServer = update.position,
                      receivedAt = now
                    ))
                  }
                } else {
                  val position = if (update.direction.isMoving) old.position else update.position
                  Some(old.copy(
                    position = position,
                    lastPositionFromServer = update.position,
                    smoothedPositionAtLastServerUpdate = old.position,
                    direction = update.direction,
                    lookDirection = update.lookDirection,
                    receivedAt = now,
                    directionLastChangedAt = if (old.lookDirection == update.lookDirection) old.directionLastChangedAt else now
                  ))
                }
              case None =>
                Some(PlayerState(
                  position = update.position,
                  lastPositionFromServer = update.position,
                  smoothedPositionAtLastServerUpdate = update.position,
                  direction = update.direction,
                  lookDirection = update.lookDirection,
                  receivedAt = now,
                  directionLastChangedAt = now
                ))
            }
          }
        case PlayerDisconnected(id) =>
          playerStates.remove(id)
        case _: Pong | _: SessionEstablished =>
          throw new RuntimeException("This should not happen")

      }
      nextPlayerEvent = eventReceiver.poll()
    }
  }

  private def render(now: Float): Unit = {
    glClearColor(0.0f, 0.0f, 0.0f, 0.0f)
    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT | GL_STENCIL_BUFFER_BIT)

    nvgBeginFrame(nvg, windowSize.x, windowSize.y, pixelRatio)
    nvgFontSize(nvg, 20)

    val camera = Camera.centerOn(playerStates.get(playerId).fold(V2.zero[Float])(_.position), gameMap.size, windowGeometry)

    camera.visibleTiles.foreach {
      case V2(x, y) =>
        gameMap.tile(x, y).foreach { tile =>
          val tileRect = Rect(xy = V2(x.toFloat, y.toFloat), wh = V2(1.0f, 1.0f))
          val texturePosition = V2(tile.tileIndex, 2)
          tileAtlas.render(
            nvg,
            rectOnScreen = camera.transformRect(tileRect),
            positionOnTexture = texturePosition,
            scaleFactor = windowGeometry.scaleFactor
          )
        }
    }

    nvgTextAlign(nvg, NVG_ALIGN_TOP | NVG_ALIGN_CENTER)
    playerStates.foreach {
      case (id, player) =>
        val sizeInTiles = V2(1, 2)
        val playerRect = Rect(xy = player.position - V2(0.0f, 1.0f), wh = sizeInTiles.map(_.toFloat))
        camera.transformVisibleRect(playerRect).foreach { screenRect =>
          tileAtlas.render(
            nvg,
            rectOnScreen = screenRect,
            positionOnTexture = V2(player.spriteIndexAt(now), 0),
            scaleFactor = windowGeometry.scaleFactor
          )

          val playerNamePoint = camera.transformPoint(player.position + V2(0.5f, -1.2f))
          renderText(nvg, playerNamePoint.x, playerNamePoint.y, id.toString.take(4))
        }
    }

    if (debugShowHitbox) {
      nvgStrokeColor(nvg, GlfwUtil.color(1, 1, 1, 0.6))
      playerStates.foreach {
        case (_, player) =>
          val hitbox = Constants.playerHitbox.translate(player.lastPositionFromServer)
          camera.transformVisibleRect(hitbox).foreach {
            case Rect(V2(x, y), V2(w, h)) =>
              nvgBeginPath(nvg)
              nvgRect(nvg, x, y, w, h)
              nvgStroke(nvg)
          }
      }
    }

    nvgTextAlign(nvg, NVG_ALIGN_TOP | NVG_ALIGN_LEFT)
    renderText(nvg, 0, 0, lastPingRtt)

    nvgEndFrame(nvg)
  }

  private def renderText(nvg: Long, x: Float, y: Float, str: String): Unit = {
    nvgFillColor(nvg, GlfwUtil.color(0, 0, 0, 0.7))
    val _ = nvgText(nvg, x + 1, y + 1, str)

    nvgFillColor(nvg, GlfwUtil.color(1, 1, 1))
    val _ = nvgText(nvg, x, y, str)
  }
}
