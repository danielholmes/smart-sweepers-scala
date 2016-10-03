package org.danielholmes.smartsweepers.ui

import java.util.{Timer, TimerTask}

import org.danielholmes.smartsweepers.nn.NeuralNet
import org.danielholmes.smartsweepers.sim._

import scala.swing.{BoxPanel, Graphics2D, Orientation}
import scala.util.Random

class SimRunPanel(
  private val simSize: Size,
  private val numMines: Int,
  private val framesPerSecond: Int,
  private val brains: List[NeuralNet]
) extends BoxPanel(Orientation.Vertical) {
  private val randomiser = new Random()

  private var sim = new Simulation(
    simSize,
    brains.map(b => new MineSweeper(b, simSize.createRandomPosition(), randomiser.nextDouble() * Math.PI * 2)),
    List.fill(numMines) { Mine(Vector2D(randomiser.nextDouble * simSize.width, randomiser.nextDouble * simSize.height)) }
  )

  private val displayPanel = new SimDisplayPanel(sim)
  private val timer = new Timer
  private val stepMillis = 1000 / framesPerSecond

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
