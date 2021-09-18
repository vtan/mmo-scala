package mmo.client.game

import mmo.client.graphics.{GlfwEvent, GlfwUtil}
import mmo.client.network.Connection
import mmo.common.api.{PlayerCommand, SessionEstablished}
import mmo.common.linear.V2

import org.lwjgl.nanovg.NanoVG._
import org.lwjgl.opengl.GL11C._
import scala.concurrent.Future
import scala.util.{Failure, Success}

class ConnectingStage(
  resources: Resources,
  reconnect: Boolean = false
) extends AppStage {

  import resources.nvg

  private val connectionFuture: Future[Connection] = {
    val host = sys.env.getOrElse("MMO_HOST", "localhost")
    val port = sys.env.get("MMO_PORT").flatMap(_.toIntOption).getOrElse(10001)
    println(s"Connecting to $host:$port")
    Connection(host, port)
  }

  private var connection: Option[Connection] = None
  private var reconnectAfter: Option[Double] = None

  override def frame(events: List[GlfwEvent], mousePosition: V2[Double], now: Double, dt: Double): Option[() => AppStage] = {
    reconnectAfter match {
      case Some(t) if now >= t =>
        Some(() => new ConnectingStage(resources, reconnect = true))

      case Some(t) =>
        renderText(f"Connection failed, retrying in ${t - now}%.1f s")
        None

      case None =>
        connection match {
          case None =>
            renderText(if (reconnect) "Connection failed, reconnecting..." else "Connecting...")
            connectionFuture.value match {
              case Some(Success(conn)) =>
                conn.commandSender.offer(PlayerCommand.InitiateSession())
                connection = Some(conn)
                None

              case Some(Failure(ex)) => handleError(ex, now)

              case None => None
            }

          case Some(conn) =>
            renderText("Establishing session...")
            conn.eventReceiver.poll() match {
              case null => None
              case event: SessionEstablished => Some(() => Game(resources, conn, event))
              case _ => handleError(new RuntimeException("Unexpected first event"), now)
            }
        }
    }
  }

  private def handleError(ex: Throwable, now: Double): None.type = {
    ex.printStackTrace()
    reconnectAfter = Some(now + 3.0)
    None
  }

  private def renderText(text: String): Unit = {
    glClearColor(0.0f, 0.0f, 0.0f, 0.0f)
    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT | GL_STENCIL_BUFFER_BIT)

    resources.nvgBeginFrame()
    nvgFontSize(nvg, 24)

    nvgFillColor(nvg, white)
    nvgTextAlign(nvg, NVG_ALIGN_TOP | NVG_ALIGN_LEFT)
    val _ = nvgText(nvg, 10.0f, 10.0f, text)

    nvgEndFrame(nvg)
  }

  private val white = GlfwUtil.color(1, 1, 1)
}
