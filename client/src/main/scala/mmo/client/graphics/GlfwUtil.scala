package mmo.client.graphics

import mmo.common.linear.V2

import org.lwjgl.glfw.GLFW._
import org.lwjgl.glfw.GLFWErrorCallback
import org.lwjgl.nanovg.NVGColor
import org.lwjgl.system.MemoryStack.stackPush
import org.lwjgl.system.MemoryUtil.NULL

object GlfwUtil {

  def createWindow(requestedWindowSize: V2[Int]): Long = {
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

  def getWindowGeometry(window: Long): (V2[Double], Double) = {
    val windowWidth = Array(0)
    val windowHeight = Array(0)
    val framebufferWidth = Array(0)
    val framebufferHeight = Array(0)
    glfwGetWindowSize(window, windowWidth, windowHeight)
    glfwGetFramebufferSize(window, framebufferWidth, framebufferHeight)
    val pixelRatio = framebufferWidth(0).toDouble / windowWidth(0).toDouble

    (V2(windowWidth(0).toDouble, windowHeight(0).toDouble), pixelRatio)
  }

  def color(r: Double, g: Double, b: Double, a: Double = 1.0): NVGColor = {
    val c = NVGColor.create()
    c.r(r.toFloat)
    c.g(g.toFloat)
    c.b(b.toFloat)
    c.a(a.toFloat)
  }
}
