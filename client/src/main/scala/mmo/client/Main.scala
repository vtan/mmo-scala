package mmo.client

import com.sksamuel.avro4s.{AvroInputStream, AvroOutputStream}
import java.io.{ByteArrayInputStream, ByteArrayOutputStream, InputStream, OutputStream}
import java.net.Socket
import java.nio.ByteBuffer
import java.util.concurrent.atomic.AtomicReference
import java.util.UUID
import java.util.concurrent.ArrayBlockingQueue
import mmo.client.common.V2
import mmo.client.graphics.TileAtlas
import mmo.common.{PlayerCommand, PlayerDisconnected, PlayerEvent, PlayerPositionChanged}
import org.lwjgl.glfw.Callbacks._
import org.lwjgl.glfw.GLFW._
import org.lwjgl.glfw._
import org.lwjgl.nanovg.NanoVG._
import org.lwjgl.nanovg.NanoVGGL3._
import org.lwjgl.nanovg.NVGColor
import org.lwjgl.opengl._
import org.lwjgl.opengl.GL11C._
import org.lwjgl.system.MemoryStack._
import org.lwjgl.system.MemoryUtil._
import scala.util.{Failure, Success, Try}

object Main {
  private val requestedWindowSize = V2(960, 720)

  private val eventsRef = new AtomicReference(List.empty[GlfwEvent])

  private val playerEventQueue = new ArrayBlockingQueue[PlayerEvent](256)

  def main(args: Array[String]): Unit = {
    val socket = new Socket("localhost", 10001)
    val inputStream = socket.getInputStream
    val outputStream = socket.getOutputStream

    new Thread(() => {
      try {
        while (true) {
          deserializeEvent(inputStream) match {
            case Success(event) => playerEventQueue.add(event)
            case Failure(exception) => exception.printStackTrace()
          }
        }
      } catch {
        case _: Throwable => () // TODO handle disconnect if not quitting
      }
    }, "socket-reader").start()

    val window = createWindow()

    GL.createCapabilities()
    val nvg = nvgCreate(NVG_ANTIALIAS)

    glfwSetKeyCallback(window,
      (window: Long, key: Int, scancode: Int, action: Int, mods: Int) => {
        val event = KeyboardEvent(
          key = key,
          action = action match {
            case GLFW_PRESS => KeyboardEvent.Press
            case GLFW_RELEASE => KeyboardEvent.Release
            case GLFW_REPEAT => KeyboardEvent.Repeat
            case unknown => throw new RuntimeException(s"Unexpected GLFW keyboard action: $unknown")
          }
        )
        val _ = eventsRef.updateAndGet(event :: _)
        ()
      }
    )

    render(window, nvg, outputStream)

    inputStream.close()
    glfwFreeCallbacks(window)
    glfwDestroyWindow(window)
    glfwTerminate()
    glfwSetErrorCallback(null).free()
  }

  private def createWindow(): Long = {
    GLFWErrorCallback.createPrint(System.err).set
    if (!glfwInit) {
      throw new IllegalStateException("Unable to initialize GLFW")
    }
    glfwDefaultWindowHints()
    glfwWindowHint(GLFW_CONTEXT_VERSION_MAJOR, 3)
    glfwWindowHint(GLFW_CONTEXT_VERSION_MINOR, 2)
    glfwWindowHint(GLFW_OPENGL_PROFILE, GLFW_OPENGL_CORE_PROFILE)
    glfwWindowHint(GLFW_VISIBLE, GLFW_FALSE)
    glfwWindowHint(GLFW_RESIZABLE, GLFW_FALSE)

    val window = glfwCreateWindow(requestedWindowSize.x, requestedWindowSize.y, "Hello World!", NULL, NULL)
    if (window == NULL) {
      throw new RuntimeException("Failed to create the GLFW window")
    }

    val stack = stackPush
    try {
      val pWidth = stack.mallocInt(1)
      val pHeight = stack.mallocInt(1)
      glfwGetWindowSize(window, pWidth, pHeight)
      val vidmode = glfwGetVideoMode(glfwGetPrimaryMonitor)
      val x = (vidmode.width - pWidth.get(0)) / 2
      val y = (vidmode.height - pHeight.get(0)) / 2
      glfwSetWindowPos(window, x, y)
    } finally {
      if (stack != null) stack.close()
    }

    glfwMakeContextCurrent(window)
    glfwSwapInterval(1)
    glfwShowWindow(window)

    window
  }

  private def render(window: Long, nvg: Long, outputStream: OutputStream): Unit = {
    val scaleFactor = 3

    val font = nvgCreateFont(nvg, "Roboto", "assets/roboto/Roboto-Regular.ttf")
    nvgFontFaceId(nvg, font)


    val characterAtlas = TileAtlas.loadFromFile(nvg, "assets/charset.png")

    val windowWidth = Array(0)
    val windowHeight = Array(0)
    val framebufferWidth = Array(0)
    val framebufferHeight = Array(0)
    glfwGetWindowSize(window, windowWidth, windowHeight)
    glfwGetFramebufferSize(window, framebufferWidth, framebufferHeight)
    val pixelRatio = framebufferWidth(0).toFloat / windowWidth(0).toFloat

    val screenSize = V2(windowWidth(0).toFloat, windowHeight(0).toFloat)

    var lastFrameTime = 0.0
    var fpsWindowTotalTime = 0.0
    var fpsWindowLength = 0

    final case class PlayerState(
      position: V2[Int],
      previousPosition: V2[Int]
    ) {
      val directionIndex: Int = {
        val (down, up, right, left) = (0, 1, 2, 3)
        val V2(dx, dy) = position - previousPosition
        if (dx < 0) {
          left
        } else if (dx > 0) {
          right
        } else if (dy < 0) {
          up
        } else {
          down
        }
      }
    }
    val playerStates = scala.collection.mutable.Map.empty[UUID, PlayerState]

    while (!glfwWindowShouldClose(window)) {
      val events = eventsRef.getAndSet(Nil).reverse
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
            serializeCommand(PlayerCommand.Move(dx, dy), outputStream)
          }
        case _ => ()
      }

      var nextPlayerEvent = playerEventQueue.poll()
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
        nextPlayerEvent = playerEventQueue.poll()
      }

      glClearColor(0.0f, 0.0f, 0.0f, 0.0f)
      glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT | GL_STENCIL_BUFFER_BIT)
      val _ = {
        nvgBeginFrame(nvg, screenSize.x, screenSize.y, pixelRatio)

        nvgFillColor(nvg, createColor(0.0, 1.0, 1.0))
        playerStates.foreach {
          case (id, player) =>
              characterAtlas.render(
                nvg,
                screenPosition = (scaleFactor * TileAtlas.tileSize) *: (player.position - V2(0, 1)),
                tilePosition = V2(player.directionIndex, 0),
                tileCount = V2(1, 2),
                scaleFactor = scaleFactor
              )
        }

        nvgEndFrame(nvg)
      }

      glfwSwapBuffers(window)
      glfwPollEvents()

      val now = glfwGetTime()
      fpsWindowTotalTime += now - lastFrameTime
      lastFrameTime = now
      fpsWindowLength += 1

      if (fpsWindowLength == 16) {
        val fps = fpsWindowLength / fpsWindowTotalTime
        glfwSetWindowTitle(window, String.format("FPS: %.0f", fps))
        fpsWindowTotalTime = 0.0
        fpsWindowLength = 0
      }
    }
  }

  private def createColor(r: Double, g: Double, b: Double, a: Double = 1): NVGColor = {
    val c = NVGColor.create()
    c.r(r.toFloat)
    c.g(g.toFloat)
    c.b(b.toFloat)
    c.a(a.toFloat)
  }

  private def deserializeEvent(inputStream: InputStream): Try[PlayerEvent] = {
    val messageSize = ByteBuffer.wrap(inputStream.readNBytes(4)).getInt
    val bytes = new ByteArrayInputStream(inputStream.readNBytes(messageSize))
    val avro = AvroInputStream.binary[PlayerEvent].from(bytes).build(PlayerEvent.avroSchema)
    val result = avro.tryIterator.toSeq match {
      case result +: _ => result
      case _ => Failure(new RuntimeException("Empty event message"))
    }
    avro.close()
    result
  }

  private def serializeCommand(command: PlayerCommand, outputStream: OutputStream): Unit = {
    val payloadStream = new ByteArrayOutputStream()
    val avro = AvroOutputStream.binary[PlayerCommand].to(payloadStream).build()
    avro.write(command)
    avro.close()
    val sizeBuffer = ByteBuffer.allocate(4)
    sizeBuffer.putInt(payloadStream.size)
    outputStream.write(sizeBuffer.array())
    payloadStream.writeTo(outputStream)
  }
}
