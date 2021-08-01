package mmo.client.graphics

import org.lwjgl.glfw.GLFW.glfwSetWindowTitle

class FpsCounter {
  private var lastFrameTime: Double = 0.0
  private var fpsWindowTotalTime: Double = 0.0
  private var fpsWindowLength: Int = 0

  def endOfFrame(window: Long, now: Double): Unit = {
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
