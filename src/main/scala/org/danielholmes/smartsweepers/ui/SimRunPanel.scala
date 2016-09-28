package org.danielholmes.smartsweepers.ui

import java.util.{Timer, TimerTask}

import org.danielholmes.smartsweepers.nn.NeuralNet
import org.danielholmes.smartsweepers.original.{CParams, Utils}
import org.danielholmes.smartsweepers.sim.{MineSweeper, Simulation, Vector2D}

import scala.swing.{BoxPanel, Graphics2D, Orientation}

class SimRunPanel(private val brains: List[NeuralNet]) extends BoxPanel(Orientation.Vertical) {
  private var sim = new Simulation(
    brains.map(b => new MineSweeper(b)),
    List.fill(CParams.iNumMines) { Vector2D(Utils.RandFloat * CParams.WindowWidth, Utils.RandFloat * CParams.WindowHeight) }
  )

  private val displayPanel = new SimDisplayPanel(sim)
  private val timer = new Timer
  private val stepMillis = 1000 / CParams.iFramesPerSecond

  contents += displayPanel

  runStep()

  def dispose() = timer.cancel()

  private def runStep(): Unit = {
    val start = System.currentTimeMillis()
    sim = sim.update()
    displayPanel.sim = sim
    val took = System.currentTimeMillis() - start

    if (took < stepMillis) {
      delay(runStep, stepMillis - took)
    } else {
      runStep()
    }
  }

  private def delay(f: () => Unit, n: Long) = timer.schedule(new TimerTask() { def run() = f() }, n)

  override def paint(g: Graphics2D): Unit = {
    super.paint(g)

    displayPanel.minimumSize = size
  }
}
