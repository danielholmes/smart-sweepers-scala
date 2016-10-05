package org.danielholmes.smartsweepers.sim

case class Rock(position: Vector2D) extends SimItem {
  val size = Rock.Size

  override def update(sim: Simulation): SimItem = this
}

object Rock {
  val Size = 2
}
