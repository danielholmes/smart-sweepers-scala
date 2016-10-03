package org.danielholmes.smartsweepers.ga

class FixedFitness(private val fitness: Double) extends Fitness {
  override def calculate(population: List[Genome]): List[GenomeResult] = {
    population.map(p => GenomeResult(p, fitness))
  }
}
