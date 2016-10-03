package org.danielholmes.smartsweepers.ga

import scala.annotation.tailrec
import scala.util.Random

class GeneticAlgorithmEnvironment(
  private val mutationRate: Double,
  private val crossoverRate: Double,
  private val maxPerturbation: Double,
  private val numElites: Int,
  private val numEliteCopies: Int,
  private val fitness: Fitness
) {
  require(crossoverRate >= 0 && crossoverRate <= 1)
  require(mutationRate >= 0 && mutationRate <= 1)

  private val randomiser = new Random

  def runGeneration(population: List[Genome]): GenerationResults = {
    val performance = fitness.calculate(population)
    GenerationResults(performance, createNextPopulation(performance))
  }

  private def createNextPopulation(performance: List[GenomeResult]): List[Genome] = {
    val totalFitness = performance.map(_.fitness).sum

    @tailrec
    def reproduce(current: List[Genome]): List[Genome] = {
      if (current.size >= performance.size) {
        current
      } else {
        val mum: Genome = selectParentByRoulette(performance, totalFitness)
        val dad: Genome = selectParentByRoulette(performance, totalFitness)
        val (baby1, baby2) = crossover(mum.weights, dad.weights)

        reproduce(current ++ List(baby1, baby2).map(b => Genome(mutate(b))))
      }
    }

    reproduce(applyEugenics(performance))
  }

  private def crossover(mum: List[Double], dad: List[Double]): (List[Double], List[Double]) = {
    if (shouldntHappen(crossoverRate) || mum == dad) {
      return (mum, dad)
    }

    val crossoverPoint = Math.floor(randomiser.nextDouble * Math.min(mum.size, dad.size)).toInt

    (
      mum.slice(0, crossoverPoint) ++ dad.slice(crossoverPoint, dad.size),
      dad.slice(0, crossoverPoint) ++ mum.slice(crossoverPoint, mum.size)
    )
  }

  private def mutate(weights: List[Double]): List[Double] = {
    weights.map(w => {
      if (shouldntHappen(mutationRate)) {
        w
      } else {
        w + ((randomiser.nextDouble - randomiser.nextDouble) * maxPerturbation)
      }
    })
  }

  private def selectParentByRoulette(population: List[GenomeResult], totalFitness: Double): Genome = {
    require(population.nonEmpty)

    val fitnessTarget: Double = randomiser.nextDouble * totalFitness

    @tailrec
    def findParentByRoulette(from: List[GenomeResult], cumulativeFitness: Double): Genome = {
      from match {
        case Nil => throw new RuntimeException("Invalid state")
        case x :: xs => {
          val newFitness = cumulativeFitness + x.fitness
          if (newFitness >= fitnessTarget) {
            x.genome
          } else {
            findParentByRoulette(xs, newFitness)
          }
        }
      }
    }

    findParentByRoulette(population, 0)
  }

  private def applyEugenics(population: List[GenomeResult]): List[Genome] = {
    require(numEliteCopies >= 0)
    require(numElites >= 0)
    val best = population.sortBy(_.fitness).slice(population.size - numElites, population.size).map(_.genome)
    List.fill(numEliteCopies)(best).flatten
  }

  private def shouldntHappen(ratio: Double): Boolean = {
    require(ratio >= 0)
    require(ratio <= 1)
    randomiser.nextDouble > ratio
  }
}
