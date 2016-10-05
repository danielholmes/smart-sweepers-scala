package org.danielholmes.smartsweepers.ga

import org.danielholmes.smartsweepers.nn.NeuralNetFactory
import org.danielholmes.smartsweepers.sim._

import scala.annotation.tailrec
import scala.util.Random

abstract class SimulationFitness(
  private val size: Size,
  private val numTicks: Int,
  private val numMines: Int,
  private val numRocks: Int,
  private val neuralNetFactory: NeuralNetFactory
) extends Fitness {
  private val randomiser = new Random()

  override def calculate(population: List[Genome]): List[GenomeResult] = {
    val sim = Simulation(
      size,
      population.map(_.weights)
        .map(neuralNetFactory.createFromWeights)
        .map(MineSweeper(_, size.createRandomPosition(), randomiser.nextDouble() * Math.PI * 2))
        .map(_.asInstanceOf[SimItem]) ++
        List.fill(numMines) { Mine(size.createRandomPosition()) } ++
        List.fill(numRocks) { Rock(size.createRandomPosition()) }
    )

    createResults(runSim(0, sim))
  }

  protected def createResults(sim: Simulation): List[GenomeResult]

  @tailrec
  private def runSim(current: Int, sim: Simulation): Simulation = {
    if (current == numTicks) {
      sim
    } else {
      runSim(current + 1, sim.update())
    }
  }
}
