package org.danielholmes.nnmine.ga

import org.danielholmes.smartsweepers.ga.{GeneticAlgorithmEnvironment, Genome, LegacyFitness}
import org.scalatest._

class GeneticAlgorithmEnvironmentSpec extends FlatSpec with Matchers {
  "GeneticAlgorithmEnvironment" should "mutate" in {
    val ga = new GeneticAlgorithmEnvironment(
      crossoverRate = 0,
      mutationRate = 1,
      numElites = 0,
      numEliteCopies = 1,
      maxPerturbation = 5,
      fitness = new LegacyFitness()
    )
    val results = ga.runGeneration(List(Genome(List.fill(44) { 10 }, 0.0), Genome(List.fill(44) { 10 }, 0.0)))
    val mutatedWeights = results.nextPopulation.flatMap(_.weights)
    for (w <- mutatedWeights) {
      w should be <= 15.0
      w should be >= 5.0
    }
    // Sometimes technically could have duplicates, but should be very rare
    mutatedWeights.toSet.size should be (88)
  }

  it should "keep elites in next population" in {
    val ga = new GeneticAlgorithmEnvironment(
      crossoverRate = 0,
      mutationRate = 0,
      numElites = 2,
      numEliteCopies = 2,
      maxPerturbation = 5,
      fitness = new LegacyFitness()
    )
    val best1 = Genome(List.fill(44) { 10 }, 1.0)
    val best2 = Genome(List.fill(44) { 10 }, 0.9)
    val results = ga.runGeneration(List(
      best1,
      Genome(List.fill(44) { 9 }, 0.8),
      best2,
      Genome(List.fill(44) { 8 }, 0.8)
    ))
    results.nextPopulation should contain theSameElementsAs List(best1, best1, best2, best2)
  }
}
