package org.danielholmes.smartsweepers.ga

import org.danielholmes.smartsweepers.Utils.{RandFloat, RandInt}
import org.danielholmes.smartsweepers.{CParams, Utils}

import scala.util.Random

class GeneticAlgorithmEnvironment(
  private val mutationRate: Double,
  private val crossoverRate: Double,
  private val genomeLength: Int,
  private val numElites: Int,
  private val numEliteCopies: Int
) {
  require(crossoverRate >= 0 && crossoverRate <= 1)
  require(mutationRate >= 0 && mutationRate <= 1)

  private val randomiser = new Random

  private def crossover(mum: List[Double], dad: List[Double]): (List[Double], List[Double]) = {
    if (shouldntHappen(crossoverRate) || mum == dad) {
      return (mum, dad)
    }

    //val crossoverPoint = RandInt(0, genomeLength - 1)
    val crossoverPoint = Math.floor(randomiser.nextDouble * genomeLength).toInt

    (
      mum.slice(0, crossoverPoint) ++ dad.slice(crossoverPoint, dad.size),
      dad.slice(0, crossoverPoint) ++ mum.slice(crossoverPoint, mum.size)
    )
  }

  private def mutate(weights: List[Double]): List[Double] = {
    weights.map(w => {
      if (RandFloat < mutationRate) {
        w
      } else {
        w + (Utils.RandomClamped * CParams.dMaxPerturbation)
      }
    })
  }

  private def selectChromosomeByRoulette(population: List[Genome], totalFitness: Double): Genome = {
    require(population.nonEmpty)

    val Slice: Double = RandFloat * totalFitness
    var TheChosenOne: Genome = null
    var FitnessSoFar: Double = 0
    var i: Int = 0
    var done = false
    while (i < population.size && !done) {
      {
        FitnessSoFar += population(i).fitness
        //if the fitness so far > random number return the chromo at
        //this point
        if (FitnessSoFar >= Slice) {
          TheChosenOne = population(i)
          done = true
        }
      }
      {
        i += 1; i
      }
    }
    TheChosenOne
  }

  private def applyEugenics(population: List[Genome]): List[Genome] = {
    require(numEliteCopies >= 0)
    require(numElites >= 0)
    val best = population.slice(population.size - numElites, population.size)
    List.fill(numEliteCopies)(best).flatten
  }

  def runGeneration(population: List[Genome]): GenerationResults = {
    var nextPopulation: List[Genome] = applyEugenics(population)
    val totalFitness = population.map(_.fitness).sum

    while (nextPopulation.size < population.size) {
      val mum: Genome = selectChromosomeByRoulette(population, totalFitness)
      val dad: Genome = selectChromosomeByRoulette(population, totalFitness)

      val (baby1, baby2) = crossover(mum.weights, dad.weights)

      val baby1Mutated = mutate(baby1)
      val baby2Mutated = mutate(baby2)

      nextPopulation = nextPopulation :+ Genome(baby1Mutated, 0)
      nextPopulation = nextPopulation :+ Genome(baby2Mutated, 0)
    }

    val performance = population.map(p => GenomeResult(p, p.fitness))
    GenerationResults(performance, nextPopulation)
  }

  def shouldntHappen(ratio: Double): Boolean = {
    require(ratio >= 0)
    require(ratio <= 1)
    randomiser.nextDouble > ratio
  }
}
