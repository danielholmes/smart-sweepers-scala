package org.danielholmes.smartsweepers.sim

import org.danielholmes.smartsweepers.original.CParams

import scala.util.Random

class Simulation(private val size: Size, var sweepers: List[MineSweeper], var mines: List[Vector2D]) {
  private val randomiser = new Random

  def update(): Simulation = {
    for (s <- sweepers) {
      s.update(mines)

      val grabHit: Int = s.checkForMine(mines, CParams.dMineScale)
      if (grabHit >= 0) {
        s.incrementFitness()
        mines = mines.slice(0, grabHit) ++
          List(Vector2D(randomiser.nextDouble * CParams.WindowWidth, randomiser.nextDouble * CParams.WindowHeight)) ++
          mines.slice(grabHit + 1, mines.size)
      }
    }

    this
  }
}
