package org.danielholmes.smartsweepers.ga

case class GenerationResults(performance: List[GenomeResult], nextPopulation: List[Genome]) {
  lazy val fittestResult = performance.maxBy(_.fitness)
  lazy val maxFitness = fittestResult.fitness

  private lazy val totalFitness = performance.map(_.fitness).sum
  lazy val averageFitness = totalFitness / performance.size
}
