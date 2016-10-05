package org.danielholmes.smartsweepers.ga

import org.danielholmes.smartsweepers.nn.NeuralNetFactory
import org.danielholmes.smartsweepers.sim.{Simulation, Size}

class CollectMinesAvoidRocksFitness(
  private val size: Size,
  private val numTicks: Int,
  private val numMines: Int,
  private val numRocks: Int,
  private val neuralNetFactory: NeuralNetFactory
) extends SimulationFitness(size, numTicks, numMines, numRocks, neuralNetFactory) {
  override protected def createResults(sim: Simulation): List[GenomeResult] =
    sim.sweepers.map(s => GenomeResult(Genome(s.brain.weights), s.numMinesCollected - s.numRocksHit))
}
