package org.danielholmes.smartsweepers.ga

import org.danielholmes.smartsweepers.nn.NeuralNetFactory
import org.danielholmes.smartsweepers.sim._

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
      population.map(_.weights)
        .map(neuralNetFactory.createFromWeights)
        .map(new MineSweeper(_, size.createRandomPosition(), randomiser.nextDouble() * Math.PI * 2)),
      mines = List.fill(numMines) { Mine(size.createRandomPosition()) }
    )

    val finalSim = runSim(0, sim)

    finalSim.sweepers.map(s => GenomeResult(Genome(s.brain.weights), s.numMinesCollected))
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
