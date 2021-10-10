package mmo.client.game

import mmo.client.graphics.{GlfwEvent, GlfwUtil, KeyboardEvent, MouseButtonEvent, ScreenFader}
import mmo.client.network.Connection
import mmo.common.api._
import mmo.common.linear.{Rect, V2}
import mmo.common.map.GameMap

import org.lwjgl.glfw.GLFW._
import org.lwjgl.nanovg.NanoVG._
import org.lwjgl.nanovg.NVGColor
import org.lwjgl.opengl.GL11C._
import scala.collection.mutable

object Game {
  def apply(
    resources: Resources,
    connection: Connection,
    sessionEstablished: SessionEstablished
  ): Game = new Game(
    resources = resources,
    connection = connection,
    playerId = sessionEstablished.playerId,
    selfStats = sessionEstablished.playerStats,
    gameMap = sessionEstablished.compactGameMap.toGameMap,
    playerNames = mutable.Map.from(sessionEstablished.players)
  )
}

class Game(
  resources: Resources,
  connection: Connection,
  playerId: PlayerId,
  private var selfStats: PlayerStats,
  private var gameMap: GameMap,
  playerNames: mutable.Map[PlayerId, String]
) extends AppStage {

  import connection.{commandSender, eventReceiver}
  import resources.{charAtlas, nvg, tileAtlas, windowGeometry}

  private var movementKeyBits: Int = 0
  private var selfLastSentPosition: V2[Double] = V2.zero
  private var selfLastSentDirection: Direction = Direction.none

  private var lastPingSent: Double = 0.0
  private var lastPingRtt: String = ""
  private var lastEventStatsCollected: Double = 0.0
  private var lastEventStats: String = ""

  private val entityStates = mutable.Map.empty[EntityId, EntityState]
  private var camera = Camera.centerOn(entityStates.get(playerId).fold(V2.zero)(_.position), gameMap.size, windowGeometry)
  private val damageLabels = mutable.ArrayBuffer.empty[DamageLabel]

  private val screenFader = new ScreenFader(nvg)
  private val teleportFadeLength: Double = 0.8

  private var debugShowHitbox: Boolean = false

  override def init(now: Double): Unit =
    screenFader.startFadeIn(now, teleportFadeLength)

  override def frame(events: List[GlfwEvent], mousePosition: V2[Double], now: Double, dt: Double): Option[() => AppStage] =
    if (connection.isConnected) {
      update(events, mousePosition, now, dt)
      render(now)
      None
    } else {
      Some(() => new ConnectingStage(resources, reconnect = true))
    }

  private def update(events: List[GlfwEvent], mousePosition: V2[Double], now: Double, dt: Double): Unit = {
    sendPing(now)
    collectEventStats(now)
    handleEvents(events, mousePosition, now)

    val positionBeforeUpdate = (gameMap.size, entityStates.get(playerId).map(_.position))

    entityStates.filterInPlace {
      case (id, entity) =>
        id == playerId || entity.dyingAnimationStarted.forall(_ + EntityState.dyingAnimationLength > now)
    }
    entityStates.mapValuesInPlace { (entityId, entity) =>
      if (!entity.isAlive) {
        entity
      } else if (entityId == playerId) {
        updateSelfMovement(self = entity, dt = dt)
      } else if (entity.direction.isMoving) {
        updateOtherMovement(entity = entity, now = now)
      } else {
        entity
      }
    }
    damageLabels.filterInPlace(_.startTime + DamageLabel.duration > now)

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

  private def collectEventStats(now: Double): Unit =
    if (now - lastEventStatsCollected >= 1.0) {
      val stats = eventReceiver.clearStats()
      lastEventStatsCollected = now
      lastEventStats = s"evs/s: ${stats.count} (${stats.totalSize} B)"
    }

  private def handleEvents(events: List[GlfwEvent], mousePosition: V2[Double], now: Double): Unit =
    events.foreach {
      case KeyboardEvent(MovementKeyBits.Bit(bit), KeyboardEvent.Press) =>
        movementKeyBits |= bit

      case KeyboardEvent(MovementKeyBits.Bit(bit), KeyboardEvent.Release) =>
        movementKeyBits &= ~bit

      case KeyboardEvent(GLFW_KEY_F2, KeyboardEvent.Press) =>
        debugShowHitbox = !debugShowHitbox

      case MouseButtonEvent(GLFW_MOUSE_BUTTON_LEFT, MouseButtonEvent.Press) =>
        entityStates.updateWith(playerId)(_.map { player =>
          if (player.isAlive && player.attackAnimationStarted + Constants.playerAttackLength < now) {
            val target = camera.screenToPoint(mousePosition)
            val clickDirection = target - (player.position + player.appearance.collisionBox.xy)
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

  private def updateSelfMovement(self: EntityState, dt: Double): EntityState = {
    val inputDirection = MovementKeyBits.directions(movementKeyBits)
    if (!inputDirection.isMoving && inputDirection == selfLastSentDirection) {
      self
    } else {
      val distance = dt * self.speed

      def check(direction: Direction) = {
        val positionChange = distance *: direction.vector
        val newPosition = self.position + positionChange
        if (gameMap.doesRectCollide(self.appearance.collisionBox.translate(newPosition))) {
          None
        } else {
          Some(newPosition -> direction)
        }
      }

      val (newPosition, newDirection) =
        check(inputDirection)
          .orElse(inputDirection.linearComponents.map(check).collectFirst { case Some(x) => x })
          .getOrElse(self.position -> Direction.none)
      val newLookDirection = if (newDirection.isMoving) newDirection.lookDirection else self.lookDirection

      val newSelf = self.copy(position = newPosition, direction = newDirection, lookDirection = newLookDirection)
      val hasDirectionChanged = newDirection != selfLastSentDirection
      val hasMovedLongEnough = (newPosition - selfLastSentPosition).lengthSq >= 1.0
      if (hasDirectionChanged || hasMovedLongEnough) {
        sendSelfMovement(newSelf)
      } else {
        newSelf
      }
    }
  }

  private def sendSelfMovement(self: EntityState): EntityState = {
    commandSender.offer(PlayerCommand.Move(self.position, self.direction, self.lookDirection))
    this.selfLastSentPosition = self.position
    this.selfLastSentDirection = self.direction
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
      val extrapolatedMovement = ((now - entity.lastServerEventAt) * entity.speed) *: entity.direction.vector
      val position = entity.lastPositionFromServer + extrapolatedMovement
      entity.copy(position = position)
    }
  }

  private def handleServerEvent(event: PlayerEvent, now: Double): Unit =
    event match {
      case EntityPositionsChanged(positions) =>
        positions.foreach { update =>
          entityStates.updateWith(update.entityId)(_.map { old =>
            val calculateInterpolation = update.entityId != playerId
            old.applyPositionChange(update, now, calculateInterpolation)
          })
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
        screenFader.startFadeIn(now, teleportFadeLength)

      case OtherPlayerDisappeared(id) =>
        entityStates -= id

      case OtherPlayerConnected(id, name) =>
        playerNames += (id -> name)

      case OtherPlayerDisconnected(id) =>
        entityStates -= id
        playerNames -= id

      case EntitiesAppeared(entities) =>
        entities.foreach { entity =>
          entityStates(entity.id) = EntityState.newAt(entity, now)
        }

      case EntityDied(id) =>
        val _ = entityStates.updateWith(id)(_.map(_.copy(
          dyingAnimationStarted = Some(now),
          direction = Direction.none
        )))
        ()

      case EntityDamaged(id, damage, hitPoints) =>
        entityStates.get(id).foreach { entity =>
          damageLabels += DamageLabel(
            initialPosition = entity.position + V2(0.5, 0),
            startTime = now,
            label = damage.toString
          )
          entityStates(id) = entity.copy(hitPoints = hitPoints)
        }

      case change: StatsChanged =>
        selfStats = selfStats.change(change)

      case _: Pong | _: SessionEstablished =>
        throw new RuntimeException("This should not happen")
    }

  private def render(now: Double): Unit = {
    glClearColor(0.0f, 0.0f, 0.0f, 0.0f)
    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT | GL_STENCIL_BUFFER_BIT)

    resources.nvgBeginFrame()
    nvgFontSize(nvg, 20)

    renderMapTiles(front = false)
    renderHitPointBars()
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
    nvgTextAlign(nvg, NVG_ALIGN_BOTTOM | NVG_ALIGN_CENTER)
    damageLabels.foreach { label =>
      val position = label.initialPosition + V2(0, (now - label.startTime) * DamageLabel.tilePerSecond)
      camera.transformVisiblePoint(position).foreach { point =>
        renderText(nvg, point.x, point.y, label.label, colors.red)
      }
    }

    renderBar(Rect(4, resources.windowSize.y - 16 - 4, 128, 16), colors.yellow, selfStats.xp.toDouble, 100)

    screenFader.render(now, windowGeometry.windowSize)

    if (entityStates.get(playerId).exists(!_.isAlive)) {
      nvgFontSize(nvg, 60)
      nvgTextAlign(nvg, NVG_ALIGN_CENTER | NVG_ALIGN_MIDDLE)
      renderText(nvg, resources.windowSize.x / 2, resources.windowSize.y / 2, "You died")
    }

    nvgFontSize(nvg, 20)
    nvgTextAlign(nvg, NVG_ALIGN_TOP | NVG_ALIGN_LEFT)
    renderText(nvg, 0, 0, lastPingRtt)
    renderText(nvg, 0, 20, lastEventStats)
    renderText(nvg, 0, 60, "Players:")
    playerNames.values.zipWithIndex.foreach {
      case (name, i) => renderText(nvg, 0, (60 + (i + 1) * 20).toDouble, name)
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
    def isHiddenDying(entity: EntityState): Boolean =
      entity.dyingAnimationStarted.exists { t =>
        now >= t + EntityState.dyingAnimationLength ||
          ((t - now) / EntityState.dyingAnimationPeriod).toInt % 2 == 0
      }

    val entities = entityStates.toArray
    entities.sortInPlaceBy(_._2.position.y)
    entities.foreach {
      case (_, entity) if isHiddenDying(entity) => ()
      case (_, entity) =>
        val sizeInTiles = V2(1, entity.appearance.height)
        val entityRect = Rect(
          xy = entity.position - V2(0.0, entity.appearance.height.toDouble - 1.0),
          wh = sizeInTiles.map(_.toDouble)
        )
        camera.transformVisibleRect(entityRect).foreach { screenRect =>
          charAtlas.render(
            nvg,
            rectOnScreen = screenRect,
            tileIndex = entity.spriteOffsetAt(now),
            scaleFactor = windowGeometry.scaleFactor
          )
        }
    }
  }

  private def renderHitPointBars(): Unit =
    entityStates.foreach {
      case (_, entity) if entity.hitPoints < entity.maxHitPoints && entity.dyingAnimationStarted.isEmpty =>
        val rect = Rect(entity.position + V2(0, 1.15), V2(1.0, 0.15))
        camera.transformVisibleRect(rect).foreach { rect =>
          renderBar(rect, colors.red, entity.hitPoints.toDouble, entity.maxHitPoints.toDouble)
        }
      case _ => ()
    }

  private def renderBar(rect: Rect[Double], fillColor: NVGColor, filled: Double, max: Double): Unit = {
    val Rect(V2(x, y), V2(w, h)) = rect
    val fillRatio = MathUtil.clamp(filled / max, 0, 1)

    nvgFillColor(nvg, colors.darkGrey)
    nvgBeginPath(nvg)
    nvgRect(nvg, x.toFloat, y.toFloat, w.toFloat, h.toFloat)
    nvgFill(nvg)

    if (fillRatio > 0) {
      nvgFillColor(nvg, fillColor)
      nvgBeginPath(nvg)
      nvgRect(nvg, x.toFloat + 1, y.toFloat + 1, ((w - 2) * fillRatio).toFloat, h.toFloat - 2)
      nvgFill(nvg)
    }
  }

  private def renderEntityHitboxes(now: Double): Unit = {
    val regularColor = GlfwUtil.color(1, 1, 1, 0.6)
    val interpolationColor = GlfwUtil.color(1, 0, 1, 0.9)
    entityStates.foreach {
      case (id, entity) =>
        val boxes = id match {
          case _: PlayerId =>
            List(entity.appearance.collisionBox.translate(entity.lastPositionFromServer))
          case _: MobId =>
            List(
              entity.appearance.spriteBoundary.translate(entity.lastPositionFromServer),
              entity.appearance.collisionBox.translate(entity.lastPositionFromServer)
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

  private def renderText(nvg: Long, x: Double, y: Double, str: String, color: NVGColor = colors.white): Unit = {
    nvgFillColor(nvg, colors.textShadow)
    val _ = nvgText(nvg, (x + 1).toFloat, (y + 1).toFloat, str)

    nvgFillColor(nvg, color)
    val _ = nvgText(nvg, x.toFloat, y.toFloat, str)
  }

  private object colors {
    val darkGrey = GlfwUtil.color(0.2, 0.2, 0.2)
    val white = GlfwUtil.color(1, 1, 1)
    val red = GlfwUtil.color(1, 0.3, 0.3)
    val yellow = GlfwUtil.color(1, 1, 0.1)
    val textShadow = GlfwUtil.color(0, 0, 0, 0.7)
  }
}
