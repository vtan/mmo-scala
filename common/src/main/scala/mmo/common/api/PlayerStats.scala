package mmo.common.api

final case class PlayerStats(
  xp: Int
) {

  def change(statsChanged: StatsChanged): PlayerStats =
    copy(
      xp = statsChanged.xp.getOrElse(this.xp)
    )
}
