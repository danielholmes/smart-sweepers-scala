package org.danielholmes.smartsweepers.ga

import org.danielholmes.smartsweepers.nn.NeuralNetFactory
import org.danielholmes.smartsweepers.original.{CParams, Utils}
import org.danielholmes.smartsweepers.sim.{MineSweeper, Simulation, Size, Vector2D}

import scala.annotation.tailrec

class SimulationFitness(
  private val size: Size,
  private val numTicks: Int,
  private val numMines: Int,
  private val neuralNetFactory: NeuralNetFactory
) extends Fitness{
  override def calculate(population: List[Genome]): List[GenomeResult] = {
    val sim = new Simulation(
      population.map(_.weights).map(neuralNetFactory.createFromWeights).map(new MineSweeper(_)),
      mines = List.fill(CParams.iNumMines) { Vector2D(Utils.RandFloat * size.width, Utils.RandFloat * size.height) }
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
