package org.danielholmes.smartsweepers.ga

import org.danielholmes.smartsweepers.nn.NeuralNetFactory
import org.danielholmes.smartsweepers.sim.{MineSweeper, Simulation, Size, Vector2D}

import scala.annotation.tailrec
import scala.util.Random

class SimulationFitness(
  private val size: Size,
  private val numTicks: Int,
  private val numMines: Int,
  private val neuralNetFactory: NeuralNetFactory
) extends Fitness {
  private val randomiser = new Random()

  override def calculate(population: List[Genome]): List[GenomeResult] = {
    val sim = new Simulation(
      size,
      population.map(_.weights).map(neuralNetFactory.createFromWeights).map(new MineSweeper(_)),
      mines = List.fill(numMines) { Vector2D(randomiser.nextDouble * size.width, randomiser.nextDouble * size.height) }
    )

    val finalSim = runSim(0, sim)

    finalSim.sweepers.map(s => GenomeResult(Genome(s.brain.weights, 0.0), s.fitness))
  }

  @tailrec
  private def runSim(current: Int, sim: Simulation): Simulation = {
    if (current == numTicks) {
      sim
    } else {
      runSim(current + 1, sim.update())
    }
  }
}
