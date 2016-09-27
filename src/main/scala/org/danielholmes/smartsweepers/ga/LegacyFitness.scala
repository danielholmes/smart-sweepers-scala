package org.danielholmes.smartsweepers.ga

// Current issue - very first population not actually tested
@deprecated
class LegacyFitness extends Fitness {
  @Override
  def calculate(population: List[Genome]): List[GenomeResult] = population.map(p => GenomeResult(p, p.fitness))
}
