package mmo.client.game

import mmo.client.graphics.GlfwEvent
import mmo.common.linear.V2

trait AppStage {

  def init(now: Double): Unit = {
    val _ = now
  }

  def frame(events: List[GlfwEvent], mousePosition: V2[Double], now: Double, dt: Double): Option[() => AppStage]

}
