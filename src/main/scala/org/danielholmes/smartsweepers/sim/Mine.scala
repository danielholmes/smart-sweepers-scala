package org.danielholmes.smartsweepers.sim

case class Mine(position: Vector2D) extends SimItem {
  val size = Mine.Size

  override def update(sim: Simulation): SimItem = this
}

object Mine {
  val Size = 2
}
