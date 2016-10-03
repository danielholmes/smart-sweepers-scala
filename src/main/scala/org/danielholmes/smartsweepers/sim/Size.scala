package org.danielholmes.smartsweepers.sim

import scala.util.Random

case class Size(width: Int, height: Int) {
  def createRandomPosition(): Vector2D = Vector2D(
    Size.randomiser.nextDouble * width,
    Size.randomiser.nextDouble * height
  )
}

object Size {
  private val randomiser = new Random()
}
