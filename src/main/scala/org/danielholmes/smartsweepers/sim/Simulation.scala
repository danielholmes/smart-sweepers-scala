package org.danielholmes.smartsweepers.sim

import org.danielholmes.smartsweepers.original.CParams
import org.danielholmes.smartsweepers.original.Utils._

class Simulation(var sweepers: List[MineSweeper], var mines: List[Vector2D]) {
  def update(): Simulation = {
    for (s <- sweepers) {
      s.update(mines)

      val grabHit: Int = s.checkForMine(mines, CParams.dMineScale)
      if (grabHit >= 0) {
        s.incrementFitness()
        mines = mines.slice(0, grabHit) ++
          List(Vector2D(RandFloat * CParams.WindowWidth, RandFloat * CParams.WindowHeight)) ++
          mines.slice(grabHit + 1, mines.size)
      }
    }

    this
  }
}
