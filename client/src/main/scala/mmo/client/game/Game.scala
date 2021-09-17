package mmo.client.game

import mmo.client.graphics.{GlfwEvent, GlfwUtil, KeyboardEvent, MouseButtonEvent, TileAtlas}
import mmo.client.network.{CommandSender, EventReceiver}
import mmo.common.api._
import mmo.common.linear.{Rect, V2}
import mmo.common.map.GameMap

import org.lwjgl.glfw.GLFW._
import org.lwjgl.nanovg.NanoVG._
import org.lwjgl.nanovg.NanoVGGL3._
import org.lwjgl.opengl.GL11C._
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

  private var movementKeyBits: Int = 0
  private var selfLastSentPosition: V2[Double] = V2.zero

  private var lastPingSent: Double = 0.0
  private var lastPingRtt: String = ""

  private val entityStates = mutable.Map.empty[EntityId, EntityState]
  private val mobAppearances = mutable.Map.empty[MobId, EntityAppearance]
  private var camera = Camera.centerOn(entityStates.get(playerId).fold(V2.zero)(_.position), gameMap.size, windowGeometry)

  private var debugShowHitbox: Boolean = false

  def frame(events: List[GlfwEvent], mousePosition: V2[Double], now: Double, dt: Double): Unit = {
    update(events, mousePosition, now, dt)
    render(now)
  }

  private def update(events: List[GlfwEvent], mousePosition: V2[Double], now: Double, dt: Double): Unit = {
    sendPing(now)
    handleEvents(events, mousePosition, now)

    val positionBeforeUpdate = (gameMap.size, entityStates.get(playerId).map(_.position))

    entityStates.mapValuesInPlace { (entityId, entity) =>
      if (entity.direction.isMoving) {
        if (entityId == playerId) {
          updateSelfMovement(self = entity, dt = dt)
        } else {
          updateOtherMovement(entity = entity, now = now)
        }
      } else {
        entity
      }
    }

    var nextPlayerEvent = eventReceiver.poll()
    while (nextPlayerEvent != null) {
      handleServerEvent(nextPlayerEvent, now = now)
      nextPlayerEvent = eventReceiver.poll()
    }

    val positionAfterUpdate = (gameMap.size, entityStates.get(playerId).map(_.position))
    if (positionBeforeUpdate != positionAfterUpdate) {
      camera = Camera.centerOn(entityStates.get(playerId).fold(V2.zero)(_.position), gameMap.size, windowGeometry)
    }
  }

  private def sendPing(now: Double): Unit =
    if (now - lastPingSent >= 1.0) {
      commandSender.offer(PlayerCommand.Ping(System.nanoTime()))
      lastPingSent = now
      lastPingRtt = s"RTT: ${eventReceiver.lastPingNanos.get() / 1_000_000L} ms"
    }

  private def handleEvents(events: List[GlfwEvent], mousePosition: V2[Double], now: Double): Unit = {
    val oldMovementKeyBits = movementKeyBits

    events.foreach {
      case KeyboardEvent(MovementKeyBits.Bit(bit), KeyboardEvent.Press) =>
        movementKeyBits |= bit

      case KeyboardEvent(MovementKeyBits.Bit(bit), KeyboardEvent.Release) =>
        movementKeyBits &= ~bit

      case KeyboardEvent(GLFW_KEY_F2, KeyboardEvent.Press) =>
        debugShowHitbox = !debugShowHitbox

      case MouseButtonEvent(GLFW_MOUSE_BUTTON_LEFT, MouseButtonEvent.Press) =>
        entityStates.updateWith(playerId)(_.map { player =>
          if (player.attackAnimationStarted + Constants.playerAttackLength < now) {
            val target = camera.screenToPoint(mousePosition)
            val clickDirection = target - (player.position + Constants.playerHitbox.xy)
            val lookDirection = LookDirection.fromVector(clickDirection)
            sendSelfMovement(player)
            commandSender.offer(PlayerCommand.Attack(target))
            player.copy(
              attackAnimationStarted = now,
              lookDirection = lookDirection
            )
          } else {
            player
          }
        })

      case _ => ()
    }

    if (movementKeyBits != oldMovementKeyBits) {
      val newDirection = MovementKeyBits.directions(movementKeyBits)
      val _ = entityStates.updateWith(playerId)(_.map { state =>
        val lookDirection = if (newDirection.isMoving) {
          newDirection.lookDirection
        } else {
          state.lookDirection
        }
        sendSelfMovement(state.copy(direction = newDirection, lookDirection = lookDirection))
      })
    }
  }

  private def updateSelfMovement(self: EntityState, dt: Double): EntityState = {
    val normal = self.direction.vector
    val positionChange = (dt * Constants.entityTilePerSecond) *: normal
    val newPosition = self.position + positionChange
    val hitbox = Constants.playerHitbox.translate(newPosition)
    if (gameMap.doesRectCollide(hitbox)) {
      sendSelfMovement(self.copy(direction = Direction.none))
    } else {
      val updated = self.copy(position = newPosition)
      if ((newPosition - selfLastSentPosition).lengthSq >= 1.0) {
        sendSelfMovement(updated)
      } else {
        updated
      }
    }
  }

  private def sendSelfMovement(self: EntityState): EntityState = {
    commandSender.offer(PlayerCommand.Move(self.position, self.direction, self.lookDirection))
    this.selfLastSentPosition = self.position
    self
  }

  private def updateOtherMovement(entity: EntityState, now: Double): EntityState = {
    // TODO: do not update entities outside the camera
    if (entity.isInterpolatingAt(now)) {
      // Interpolating to predicted position in the near future to avoid jumping on a new update from server
      val t = (now - entity.lastServerEventAt) / EntityState.interpolationPeriod
      val position = V2.lerp(t, entity.interpolationSource, entity.interpolationTarget)
      entity.copy(position = position)
    } else {
      val extrapolatedMovement = ((now - entity.lastServerEventAt) * Constants.entityTilePerSecond) *: entity.direction.vector
      val position = entity.lastPositionFromServer + extrapolatedMovement
      entity.copy(position = position)
    }
  }

  private def handleServerEvent(event: PlayerEvent, now: Double): Unit =
    event match {
      case EntityPositionsChanged(positions) =>
        positions.foreach { update =>
          entityStates.updateWith(update.entityId) {
            case Some(old) =>
              val calculateInterpolation = update.entityId != playerId
              Some(old.applyPositionChange(update, now, calculateInterpolation))
            case None =>
              Some(EntityState.newAt(update, now))
          }
        }

      case EntityAttacked(id) =>
        val _ = entityStates.updateWith(id)(_.map(_.copy(
          attackAnimationStarted = now
        )))

      case MovementAcked(position) =>
        val _ = entityStates.updateWith(playerId)(_.map(_.copy(
          lastPositionFromServer = position,
          lastServerEventAt = now
        )))

      case Teleported(compactGameMap) =>
        gameMap = compactGameMap.toGameMap
        // Player positions (including ours) on the new map will follow in another event
        entityStates.clear()
        mobAppearances.clear()

      case OtherPlayerDisappeared(id) =>
        entityStates -= id

      case OtherPlayerConnected(id, name) =>
        playerNames += (id -> name)

      case OtherPlayerDisconnected(id) =>
        entityStates -= id
        playerNames -= id

      case MobsAppeared(mobs) =>
        mobAppearances ++= mobs

      case MobDisappeared(id) =>
        entityStates -= id
        mobAppearances -= id

      case _: Pong | _: SessionEstablished =>
        throw new RuntimeException("This should not happen")
    }

  private def render(now: Double): Unit = {
    glClearColor(0.0f, 0.0f, 0.0f, 0.0f)
    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT | GL_STENCIL_BUFFER_BIT)

    nvgBeginFrame(nvg, windowSize.x.toFloat, windowSize.y.toFloat, pixelRatio.toFloat)
    nvgFontSize(nvg, 20)

    renderMapTiles(front = false)
    renderEntities(now)
    renderMapTiles(front = true)

    if (debugShowHitbox) {
      renderEntityHitboxes(now)
      renderMapHitboxes()
    }

    nvgTextAlign(nvg, NVG_ALIGN_TOP | NVG_ALIGN_CENTER)
    entityStates.foreach {
      case (id: PlayerId, player) =>
        camera.transformVisiblePoint(player.position + V2(0.5, -1.2)).foreach { playerNamePoint =>
          renderText(nvg, playerNamePoint.x, playerNamePoint.y, playerNames.getOrElse(id, "???"))
        }
      case (_: MobId, _) => ()
    }

    nvgTextAlign(nvg, NVG_ALIGN_TOP | NVG_ALIGN_LEFT)
    renderText(nvg, 0, 0, lastPingRtt)
    renderText(nvg, 0, 40, "Players:")
    playerNames.values.zipWithIndex.foreach {
      case (name, i) => renderText(nvg, 0, (40 + (i + 1) * 20).toDouble, name)
    }

    nvgEndFrame(nvg)
  }

  private def renderMapTiles(front: Boolean): Unit =
    camera.visibleTiles.foreach {
      case V2(x, y) =>
        val offset = gameMap.offsetOf(x, y).get
        gameMap.layers.indices.foreach { layerIndex =>
          val tileIndex = gameMap.layers(layerIndex)(offset)
          if (!tileIndex.isEmpty && gameMap.frontTileIndices(tileIndex.asInt) == front) {
            val tileRect = Rect(xy = V2(x.toDouble, y.toDouble), wh = V2(1.0, 1.0))
            tileAtlas.render(
              nvg,
              rectOnScreen = camera.transformRect(tileRect),
              tileIndex = tileIndex.asInt,
              scaleFactor = windowGeometry.scaleFactor
            )
          }
        }
    }

  private def renderEntities(now: Double): Unit = {
    val entities = entityStates.toArray
    entities.sortInPlaceBy(_._2.position.y)
    entities.foreach {
      case (entityId, entity) =>
        val (entityRect, baseSpriteIndex) = entityId match {
          case PlayerId(_) =>
            val sizeInTiles = V2(1, 2)
            val rect = Rect(xy = entity.position - V2(0.0, 1.0), wh = sizeInTiles.map(_.toDouble))
            (rect, 0)
          case mobId: MobId =>
            (Rect(xy = entity.position, V2(1.0, 1.0)), mobAppearances(mobId).spriteOffset)
        }
        camera.transformVisibleRect(entityRect).foreach { screenRect =>
          charAtlas.render(
            nvg,
            rectOnScreen = screenRect,
            tileIndex = baseSpriteIndex + entity.spriteOffsetAt(now),
            scaleFactor = windowGeometry.scaleFactor
          )
        }
    }
  }

  private def renderEntityHitboxes(now: Double): Unit = {
    val regularColor = GlfwUtil.color(1, 1, 1, 0.6)
    val interpolationColor = GlfwUtil.color(1, 0, 1, 0.9)
    entityStates.foreach {
      case (id, entity) =>
        val boxes = id match {
          case _: PlayerId =>
            List(Constants.playerHitbox.translate(entity.lastPositionFromServer))
          case mobId: MobId =>
            val appearance = mobAppearances(mobId)
            List(
              appearance.spriteBoundary.translate(entity.lastPositionFromServer),
              appearance.collisionBox.translate(entity.lastPositionFromServer)
            )
        }
        boxes.foreach { box =>
          camera.transformVisibleRect(box).foreach {
            case Rect(V2(x, y), V2(w, h)) =>
              val color = if (id != playerId && entity.isInterpolatingAt(now)) {
                interpolationColor
              } else {
                regularColor
              }
              nvgStrokeColor(nvg, color)
              nvgBeginPath(nvg)
              nvgRect(nvg, x.toFloat, y.toFloat, w.toFloat, h.toFloat)
              nvgStroke(nvg)
          }
        }
      case _ => ()
    }
  }

  private def renderMapHitboxes(): Unit = {
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
