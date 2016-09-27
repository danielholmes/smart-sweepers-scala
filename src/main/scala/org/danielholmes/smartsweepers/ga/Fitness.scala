package org.danielholmes.smartsweepers.ga

trait Fitness {
  def calculate(population: List[Genome]): List[GenomeResult]
}
