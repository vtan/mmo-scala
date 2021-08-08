package mmo.client.game

import mmo.client.graphics.{GlfwEvent, GlfwUtil, KeyboardEvent, TileAtlas}
import mmo.client.network.{CommandSender, EventReceiver}
import mmo.common.api.{Constants, Direction, MovementAcked, OtherPlayerConnected, OtherPlayerDisappeared, OtherPlayerDisconnected, PlayerCommand, PlayerEvent, PlayerId, PlayerPositionsChanged, Pong, SessionEstablished, Teleported}
import mmo.common.linear.{Rect, V2}
import mmo.common.map.GameMap

import org.lwjgl.glfw.GLFW._
import org.lwjgl.nanovg.NanoVG._
import org.lwjgl.nanovg.NanoVGGL3._
import org.lwjgl.opengl.GL11C._
import scala.collection.immutable.ArraySeq
import scala.collection.mutable

object Game {
  def apply(
    window: Long,
    eventReceiver: EventReceiver,
    commandSender: CommandSender,
    sessionEstablished: SessionEstablished
  ): Game = new Game(
    window = window,
    eventReceiver = eventReceiver,
    commandSender = commandSender,
    playerId = sessionEstablished.playerId,
    gameMap = sessionEstablished.compactGameMap.toGameMap,
    playerNames = mutable.Map.from(sessionEstablished.players)
  )
}

class Game(
  window: Long,
  eventReceiver: EventReceiver,
  commandSender: CommandSender,
  playerId: PlayerId,
  var gameMap: GameMap,
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

  def update(events: List[GlfwEvent], now: Double, dt: Double): Unit = {
    sendPing(now)
    handleEvents(events)

    playerStates.mapValuesInPlace { (id, player) =>
      if (id == playerId) {
        updateSelfMovement(player = player, dt = dt)
      } else {
        updateOtherMovement(player = player, now = now)
      }
    }

    var nextPlayerEvent = eventReceiver.poll()
    while (nextPlayerEvent != null) {
      handleServerEvent(nextPlayerEvent, now = now)
      nextPlayerEvent = eventReceiver.poll()
    }
  }

  private def sendPing(now: Double): Unit =
    if (now - lastPingSent >= 1.0f) {
      commandSender.offer(PlayerCommand.Ping(System.nanoTime()))
      lastPingSent = now
      lastPingRtt = s"RTT: ${eventReceiver.lastPingNanos.get() / 1_000_000L} ms"
    }

  private def handleEvents(events: List[GlfwEvent]): Unit = {
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
      val _ = playerStates.updateWith(playerId)(_.map { state =>
        val lookDirection = if (newDirection.isMoving) {
          newDirection.lookDirection
        } else {
          state.lookDirection
        }
        commandSender.offer(PlayerCommand.Move(state.position, newDirection, lookDirection))
        state.copy(direction = newDirection, lookDirection = lookDirection)
      })
    }
  }

  private def updateSelfMovement(player: PlayerState, dt: Double): PlayerState =
    if (player.direction.isMoving) {
      val normal = player.direction.vector
      val positionChange = (dt * Constants.playerTilePerSecond) *: normal
      val newPosition = player.position + positionChange
      val hitbox = Constants.playerHitbox.translate(newPosition)
      if (gameMap.doesRectCollide(hitbox)) {
        commandSender.offer(PlayerCommand.Move(player.position, Direction.none, player.lookDirection))
        player.copy(direction = Direction.none)
      } else {
        val hasCrossedTile = newPosition.map(_.floor.toInt) - player.position.map(_.floor.toInt) != V2.zero[Int]
        if (hasCrossedTile) {
          commandSender.offer(PlayerCommand.Move(newPosition, player.direction, player.lookDirection))
        }
        player.copy(position = newPosition)
      }
    } else {
      player
    }

  private def updateOtherMovement(player: PlayerState, now: Double): PlayerState =
    if (player.direction.isMoving) {
      val interpolationStartPeriod = 0.5
      if (now - player.lastServerEventAt <= interpolationStartPeriod) {
        // Interpolating to predicted position in the near future to avoid jumping on a new update from server
        val t = (now - player.lastServerEventAt) / interpolationStartPeriod
        val target = player.lastPositionFromServer + (interpolationStartPeriod * Constants.playerTilePerSecond) *: player.direction.vector
        val position = ((1 - t) *: player.smoothedPositionAtLastServerUpdate) + (t *: target)
        player.copy(position = position)
      } else {
        val interpolatedMovement = ((now - player.lastServerEventAt) * Constants.playerTilePerSecond) *: player.direction.vector
        val position = player.lastPositionFromServer + interpolatedMovement
        player.copy(position = position)
      }
    } else {
      player
    }

  private def handleServerEvent(event: PlayerEvent, now: Double): Unit =
    event match {
      case PlayerPositionsChanged(positions) =>
        positions.foreach { update =>
          playerStates.updateWith(update.id) {
            case Some(old) =>
              if (update.id == playerId) {
                Some(old.copy(
                  position = update.position,
                  lastPositionFromServer = update.position,
                  direction = update.direction,
                  lookDirection = update.lookDirection,
                  lastServerEventAt = now,
                  directionLastChangedAt = if (old.lookDirection == update.lookDirection) old.directionLastChangedAt else now
                ))
              } else {
                val position = if (update.direction.isMoving) old.position else update.position
                Some(old.copy(
                  position = position,
                  lastPositionFromServer = update.position,
                  smoothedPositionAtLastServerUpdate = old.position,
                  direction = update.direction,
                  lookDirection = update.lookDirection,
                  lastServerEventAt = now,
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
                lastServerEventAt = now,
                directionLastChangedAt = now
              ))
          }
        }

      case MovementAcked(position) =>
        val _ = playerStates.updateWith(playerId)(_.map(_.copy(
          lastPositionFromServer = position,
          lastServerEventAt = now
        )))

      case Teleported(compactGameMap) =>
        gameMap = compactGameMap.toGameMap
        // Player positions (including ours) on the new map will follow in another event
        playerStates.clear()

      case OtherPlayerDisappeared(id) =>
        playerStates -= id

      case OtherPlayerConnected(id, name) =>
        playerNames += (id -> name)

      case OtherPlayerDisconnected(id) =>
        playerStates -= id
        playerNames -= id

      case _: Pong | _: SessionEstablished =>
        throw new RuntimeException("This should not happen")
    }

  def render(now: Double): Unit = {
    glClearColor(0.0f, 0.0f, 0.0f, 0.0f)
    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT | GL_STENCIL_BUFFER_BIT)

    nvgBeginFrame(nvg, windowSize.x.toFloat, windowSize.y.toFloat, pixelRatio.toFloat)
    nvgFontSize(nvg, 20)

    val camera = Camera.centerOn(playerStates.get(playerId).fold(V2.zero[Double])(_.position), gameMap.size, windowGeometry)

    renderMapTiles(camera, front = false)
    renderPlayers(camera, now)
    renderMapTiles(camera, front = true)

    if (debugShowHitbox) {
      renderPlayerHitboxes(camera)
      renderMapHitboxes(camera)
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

  private def renderMapTiles(camera: Camera, front: Boolean): Unit =
    camera.visibleTiles.foreach {
      case V2(x, y) =>
        val offset = gameMap.offsetOf(x, y).get
        gameMap.layers.indices.foreach { layerIndex =>
          val tileIndex = gameMap.layers(layerIndex)(offset)
          if (!tileIndex.isEmpty && gameMap.frontTileIndices(tileIndex.asInt) == front) {
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

  private def renderPlayers(camera: Camera, now: Double): Unit =
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

  private def renderPlayerHitboxes(camera: Camera): Unit = {
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

  private def renderMapHitboxes(camera: Camera): Unit = {
    nvgStrokeColor(nvg, GlfwUtil.color(1, 0, 0, 0.6))
    camera.visibleTiles.foreach {
      case V2(x, y) =>
        if (gameMap.isObstacle(x, y)) {
          val rect = camera.transformRect(Rect(x.toDouble, y.toDouble, 1.0, 1.0))
          nvgBeginPath(nvg)
          nvgRect(nvg, rect.x.toFloat, rect.y.toFloat, rect.w.toFloat, rect.h.toFloat)
          nvgStroke(nvg)
        }
    }
  }

  private def renderText(nvg: Long, x: Double, y: Double, str: String): Unit = {
    nvgFillColor(nvg, colors.textShadow)
    val _ = nvgText(nvg, (x + 1).toFloat, (y + 1).toFloat, str)

    nvgFillColor(nvg, colors.white)
    val _ = nvgText(nvg, x.toFloat, y.toFloat, str)
  }

  private object colors {
    val white = GlfwUtil.color(1, 1, 1)
    val textShadow = GlfwUtil.color(0, 0, 0, 0.7)
  }
}
