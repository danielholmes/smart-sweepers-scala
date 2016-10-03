package org.danielholmes.smartsweepers.sim

case class Mine(position: Vector2D) {
  val size = Mine.Size
}

object Mine {
  val Size = 2
}
