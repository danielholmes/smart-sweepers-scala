package org.danielholmes.smartsweepers.sim

class Simulation(private val size: Size, var sweepers: List[MineSweeper], val mines: List[Mine]) {
  def update(): Simulation = {
    var newMines = mines
    val newSweepers = sweepers.map(s => {
      var newS = s.update(newMines)

      val grabHit = newS.checkForMine(newMines)
      if (grabHit >= 0) {
        newS = newS.collectMine()
        newMines = newMines.slice(0, grabHit) ++
          List(Mine(size.createRandomPosition())) ++
          newMines.slice(grabHit + 1, newMines.size)
      }

      newS
    })

    new Simulation(size, newSweepers, newMines)
  }
}
