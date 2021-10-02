package mmo.client

import mmo.client.game.{AppStage, ConnectingStage, Resources}
import mmo.client.graphics.{FpsCounter, GlfwEvent, GlfwUtil, KeyboardEvent, MouseButtonEvent}
import mmo.common.linear.V2

import java.util.concurrent.atomic.AtomicReference
import org.lwjgl.glfw.{GLFWCursorPosCallbackI, GLFWKeyCallbackI, GLFWMouseButtonCallbackI}
import org.lwjgl.glfw.Callbacks._
import org.lwjgl.glfw.GLFW._
import org.lwjgl.opengl._

object Main {

  def main(args: Array[String]): Unit = {
    val window = GlfwUtil.createWindow(requestedWindowSize = V2(960, 720))

    GL.createCapabilities()
    val resources = new Resources(window)

    val eventsRef = new AtomicReference(List.empty[GlfwEvent])
    val mousePositionRef = new AtomicReference(V2.zero)
    glfwSetKeyCallback(window, keyCallback(eventsRef))
    glfwSetMouseButtonCallback(window, mouseButtonCallback(eventsRef))
    glfwSetCursorPosCallback(window, cursorPosCallback(mousePositionRef))

    val fpsCounter = new FpsCounter
    var now: Double = glfwGetTime()
    var lastFrameTime: Double = now
    var appStage: AppStage = new ConnectingStage(resources)

    while (!glfwWindowShouldClose(window)) {
      val events = eventsRef.getAndSet(Nil).reverse
      val nextStage = appStage.frame(events, mousePositionRef.get(), now, now - lastFrameTime)

      glfwSwapBuffers(window)
      glfwPollEvents()

      lastFrameTime = now
      now = glfwGetTime()
      fpsCounter.endOfFrame(window, now)

      nextStage.foreach { build =>
        appStage = build()
        appStage.init(now)
      }
    }

    glfwFreeCallbacks(window)
    glfwDestroyWindow(window)
    glfwTerminate()
    glfwSetErrorCallback(null).free()
  }

  private def keyCallback(events: AtomicReference[List[GlfwEvent]]): GLFWKeyCallbackI =
    (_: Long, key: Int, _: Int, action: Int, _: Int) => {
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

  private def mouseButtonCallback(events: AtomicReference[List[GlfwEvent]]): GLFWMouseButtonCallbackI =
    (_: Long, button: Int, action: Int, _: Int) => {
      val event = MouseButtonEvent(
        button = button,
        action = action match {
          case GLFW_PRESS => MouseButtonEvent.Press
          case GLFW_RELEASE => MouseButtonEvent.Release
          case unknown => throw new RuntimeException(s"Unexpected GLFW mouse button action: $unknown")
        }
      )
      val _ = events.updateAndGet(event :: _)
      ()
    }

  private def cursorPosCallback(position: AtomicReference[V2[Double]]): GLFWCursorPosCallbackI =
    (_: Long, xpos: Double, ypos: Double) => position.set(V2(xpos, ypos))
}
