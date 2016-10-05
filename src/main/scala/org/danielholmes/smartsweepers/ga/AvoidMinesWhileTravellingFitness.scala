package org.danielholmes.smartsweepers.ga

import org.danielholmes.smartsweepers.nn.NeuralNetFactory
import org.danielholmes.smartsweepers.sim.{Simulation, Size}

// Couldn't really find a balance for this - just leads to them travelling in circles. Need a different
// definition for distance travelled, like a block of positions the sweeper has been during the sim
class AvoidMinesWhileTravellingFitness(
  private val size: Size,
  private val numTicks: Int,
  private val numMines: Int,
  private val neuralNetFactory: NeuralNetFactory
) extends SimulationFitness(size, numTicks, numMines, 0, neuralNetFactory) {
  override protected def createResults(sim: Simulation): List[GenomeResult] =
    sim.sweepers.map(s => GenomeResult(Genome(s.brain.weights), s.distanceMoved - s.numMinesCollected * 500))
}
