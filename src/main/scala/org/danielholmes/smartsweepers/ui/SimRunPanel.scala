package org.danielholmes.smartsweepers.ui

import java.util.{Timer, TimerTask}

import org.danielholmes.smartsweepers.ga.GenomeResult
import org.danielholmes.smartsweepers.nn.NeuralNetFactory
import org.danielholmes.smartsweepers.sim._

import scala.swing.{BoxPanel, Graphics2D, Orientation}
import scala.util.Random

class SimRunPanel(
  private val simSize: Size,
  private val numMines: Int,
  private val framesPerSecond: Int,
  private val results: List[GenomeResult],
  private val nnFactory: NeuralNetFactory
) extends BoxPanel(Orientation.Vertical) {
  private val randomiser = new Random()

  private var sim = Simulation(
    simSize,
    results.map(_.genome)
      .map(p => nnFactory.createFromWeights(p.weights))
      .map(b => MineSweeper(b, simSize.createRandomPosition(), randomiser.nextDouble() * Math.PI * 2))
      .map(_.asInstanceOf[SimItem]) ++
      List.fill(numMines) { Mine(Vector2D(randomiser.nextDouble * simSize.width, randomiser.nextDouble * simSize.height)) }
  )

  private val displayPanel = new SimDisplayPanel(sim, results)
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
