package mmo.client

import mmo.client.game.Game
import mmo.client.graphics.{FpsCounter, GlfwEvent, GlfwUtil, KeyboardEvent}
import mmo.client.network.{CommandSenderRunnable, EventReceiverRunnable}
import mmo.common.api.{PlayerCommand, SessionEstablished}
import mmo.common.linear.V2

import java.net.Socket
import java.util.concurrent.atomic.AtomicReference
import org.lwjgl.glfw.Callbacks._
import org.lwjgl.glfw.GLFW._
import org.lwjgl.glfw.GLFWKeyCallbackI
import org.lwjgl.opengl._

object Main {

  def main(args: Array[String]): Unit = {
    val socket = {
      val host = sys.env.getOrElse("MMO_HOST", "localhost")
      val port = sys.env.get("MMO_PORT").flatMap(_.toIntOption).getOrElse(10001)
      println(s"Connecting to $host:$port")
      new Socket(host, port)
    }
    socket.setTcpNoDelay(true)
    val inputStream = socket.getInputStream
    val outputStream = socket.getOutputStream

    val eventReceiver = new EventReceiverRunnable(inputStream)
    val commandSender = new CommandSenderRunnable(outputStream)

    val eventReceiverThread = new Thread(eventReceiver, "event-receiver")
    val commandSenderThread = new Thread(commandSender, "command-sender")
    eventReceiverThread.start()
    commandSenderThread.start()
    commandSender.offer(PlayerCommand.InitiateSession())

    val window = GlfwUtil.createWindow(requestedWindowSize = V2(960, 720))

    GL.createCapabilities()

    val eventsRef = new AtomicReference(List.empty[GlfwEvent])
    glfwSetKeyCallback(window, keyCallback(eventsRef))

    val sessionEstablised = eventReceiver.take() match {
      case s: SessionEstablished => s
      case _ => throw new RuntimeException("Unexpected first event type")
    }
    val game = Game(window, eventReceiver, commandSender, sessionEstablised)

    val fpsCounter = new FpsCounter
    var now: Double = glfwGetTime()
    var lastFrameTime: Double = now

    while (!glfwWindowShouldClose(window)) {
      val events = eventsRef.getAndSet(Nil).reverse
      game.update(events, now, now - lastFrameTime)
      game.render(now)

      glfwSwapBuffers(window)
      glfwPollEvents()

      lastFrameTime = now
      now = glfwGetTime()
      fpsCounter.endOfFrame(window, now)
    }

    eventReceiverThread.interrupt()
    commandSenderThread.interrupt()
    inputStream.close()
    outputStream.close()

    glfwFreeCallbacks(window)
    glfwDestroyWindow(window)
    glfwTerminate()
    glfwSetErrorCallback(null).free()
  }

  private def keyCallback(events: AtomicReference[List[GlfwEvent]]): GLFWKeyCallbackI =
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
      val _ = events.updateAndGet(event :: _)
      ()
    }
}
