package mmo.client.game

import mmo.client.graphics.{FpsCounter, GlfwEvent, GlfwUtil, KeyboardEvent, TileAtlas}
import mmo.client.network.{CommandSender, EventReceiver}
import mmo.common.api.{Constants, Direction, PlayerCommand, PlayerConnected, PlayerDisconnected, PlayerId, PlayerPositionsChanged, Pong, SessionEstablished}
import mmo.common.linear.{Rect, V2}
import mmo.common.map.GameMap

import java.util.concurrent.atomic.AtomicReference
import org.lwjgl.glfw.GLFW._
import org.lwjgl.nanovg.NanoVG._
import org.lwjgl.nanovg.NanoVGGL3._
import org.lwjgl.opengl.GL11C._
import scala.collection.immutable.ArraySeq
import scala.collection.mutable

class Game(
  window: Long,
  glfwEventsRef: AtomicReference[List[GlfwEvent]],
  eventReceiver: EventReceiver,
  commandSender: CommandSender,
  playerId: PlayerId,
  gameMap: GameMap,
  playerNames: mutable.Map[PlayerId, String]
) {
  private val nvg: Long = nvgCreate(0)
  private val (windowSize: V2[Double], pixelRatio: Double) = GlfwUtil.getWindowGeometry(window)
  private val windowGeometry = WindowGeometry(
    windowSize = windowSize,
    scaleFactor = 3
  )

  private val charAtlas: TileAtlas = TileAtlas.loadFromFile(nvg, "assets/charset.png")
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

  private var lastPingSent: Double = 0.0f
  private var lastPingRtt: String = ""
  private val playerStates = mutable.Map.empty[PlayerId, PlayerState]
  private var debugShowHitbox: Boolean = false

  def run(): Unit = {
    val fpsCounter = new FpsCounter
    var now: Double = glfwGetTime()
    var lastFrameTime: Double = now

    while (!glfwWindowShouldClose(window)) {
      val events = glfwEventsRef.getAndSet(Nil).reverse
      update(events, now, now - lastFrameTime)

      render(now)
      glfwSwapBuffers(window)
      glfwPollEvents()

      lastFrameTime = now
      now = glfwGetTime()
      fpsCounter.endOfFrame(window, now)
    }
  }

  private def update(events: List[GlfwEvent], now: Double, dt: Double): Unit = {
    if (now - lastPingSent >= 1.0f) {
      commandSender.offer(PlayerCommand.Ping(System.nanoTime()))
      lastPingSent = now
      lastPingRtt = s"RTT: ${eventReceiver.lastPingNanos.get() / 1_000_000L} ms"
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
          if (gameMap.doesRectCollide(hitbox)) {
            commandSender.offer(PlayerCommand.Move(state.position, Direction.none, state.lookDirection))
            state.copy(direction = Direction.none)
          } else {
            val hasCrossedTile = newPosition.map(_.floor.toInt) - state.position.map(_.floor.toInt) != V2.zero[Int]
            if (hasCrossedTile) {
              commandSender.offer(PlayerCommand.Move(newPosition, state.direction, state.lookDirection))
            }
            state.copy(position = newPosition)
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
                      position = update.position,
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
        case PlayerConnected(id, name) =>
          playerNames += (id -> name)
        case PlayerDisconnected(id) =>
          playerStates -= id
          playerNames -= id
        case _: Pong | _: SessionEstablished =>
          throw new RuntimeException("This should not happen")

      }
      nextPlayerEvent = eventReceiver.poll()
    }
  }

  private def render(now: Double): Unit = {
    glClearColor(0.0f, 0.0f, 0.0f, 0.0f)
    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT | GL_STENCIL_BUFFER_BIT)

    nvgBeginFrame(nvg, windowSize.x.toFloat, windowSize.y.toFloat, pixelRatio.toFloat)
    nvgFontSize(nvg, 20)

    val camera = Camera.centerOn(playerStates.get(playerId).fold(V2.zero[Double])(_.position), gameMap.size, windowGeometry)

    camera.visibleTiles.foreach {
      case V2(x, y) =>
        val offset = gameMap.offsetOf(x, y).get
        gameMap.layers.indices.foreach { layerIndex =>
          val tileIndex = gameMap.layers(layerIndex)(offset)
          if (!tileIndex.isEmpty && !gameMap.frontTileIndices(tileIndex.asInt)) {
            val tileRect = Rect(xy = V2(x.toDouble, y.toDouble), wh = V2(1.0, 1.0))
            val texturePosition = tileIndex.toTexturePosition
            tileAtlas.render(
              nvg,
              rectOnScreen = camera.transformRect(tileRect),
              positionOnTexture = texturePosition,
              scaleFactor = windowGeometry.scaleFactor
            )
          }
        }
    }

    ArraySeq.from(playerStates.values).sortBy(_.position.y).foreach { player =>
      val sizeInTiles = V2(1, 2)
      val playerRect = Rect(xy = player.position - V2(0.0, 1.0), wh = sizeInTiles.map(_.toDouble))
      camera.transformVisibleRect(playerRect).foreach { screenRect =>
        charAtlas.render(
          nvg,
          rectOnScreen = screenRect,
          positionOnTexture = V2(player.spriteIndexAt(now), 0),
          scaleFactor = windowGeometry.scaleFactor
        )
      }
    }

    // TODO deduplicate with rendering non-front tiles

    camera.visibleTiles.foreach {
      case V2(x, y) =>
        val offset = gameMap.offsetOf(x, y).get
        gameMap.layers.indices.foreach { layerIndex =>
          val tileIndex = gameMap.layers(layerIndex)(offset)
          if (!tileIndex.isEmpty && gameMap.frontTileIndices(tileIndex.asInt)) {
            val tileRect = Rect(xy = V2(x.toDouble, y.toDouble), wh = V2(1.0, 1.0))
            val texturePosition = tileIndex.toTexturePosition
            tileAtlas.render(
              nvg,
              rectOnScreen = camera.transformRect(tileRect),
              positionOnTexture = texturePosition,
              scaleFactor = windowGeometry.scaleFactor
            )
          }
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
              nvgRect(nvg, x.toFloat, y.toFloat, w.toFloat, h.toFloat)
              nvgStroke(nvg)
          }
      }
    }

    nvgTextAlign(nvg, NVG_ALIGN_TOP | NVG_ALIGN_CENTER)
    playerStates.foreach {
      case (id, player) =>
        camera.transformVisiblePoint(player.position + V2(0.5, -1.2)).foreach { playerNamePoint =>
          renderText(nvg, playerNamePoint.x, playerNamePoint.y, playerNames.getOrElse(id, "???"))
        }
    }

    nvgTextAlign(nvg, NVG_ALIGN_TOP | NVG_ALIGN_LEFT)
    renderText(nvg, 0, 0, lastPingRtt)
    renderText(nvg, 0, 40, "Players:")
    playerNames.values.zipWithIndex.foreach {
      case (name, i) => renderText(nvg, 0, (40 + (i + 1) * 20).toDouble, name)
    }

    nvgEndFrame(nvg)
  }

  private def renderText(nvg: Long, x: Double, y: Double, str: String): Unit = {
    nvgFillColor(nvg, GlfwUtil.color(0, 0, 0, 0.7))
    val _ = nvgText(nvg, (x + 1).toFloat, (y + 1).toFloat, str)

    nvgFillColor(nvg, GlfwUtil.color(1, 1, 1))
    val _ = nvgText(nvg, x.toFloat, y.toFloat, str)
  }
}

object Game {
  def apply(
    window: Long,
    glfwEventsRef: AtomicReference[List[GlfwEvent]],
    eventReceiver: EventReceiver,
    commandSender: CommandSender,
    sessionEstablished: SessionEstablished
  ): Game = new Game(
    window = window,
    glfwEventsRef = glfwEventsRef,
    eventReceiver = eventReceiver,
    commandSender = commandSender,
    playerId = sessionEstablished.playerId,
    gameMap = sessionEstablished.compactGameMap.toGameMap,
    playerNames = mutable.Map.from(sessionEstablished.players)
  )
}
