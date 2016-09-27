package org.danielholmes.smartsweepers.ga

import org.danielholmes.smartsweepers.Utils.{RandFloat, RandInt}
import org.danielholmes.smartsweepers.{CParams, Utils}

class CGenAlg(
               var popSize: Int,
               //Try figures around 0.05 to 0.3 ish
               var mutationRate: Double,
               var crossoverRate: Double,
               var chromosomeLength: Int
) {
  require(crossoverRate >= 0 && crossoverRate <= 1)
  require(mutationRate >= 0 && mutationRate <= 1)

  private var totalFitness = 0.0
  var bestFitness = 0.0
  private var worstFitness = 99999999.0
  var population = List.fill(popSize) { Genome(List.fill(chromosomeLength) { Utils.RandomClamped }, 0) }
  private var fittestGenome: Int = 0
  private var generation: Int = 0

  private def crossover(mum: List[Double], dad: List[Double]): (List[Double], List[Double]) = {
    if (RandFloat > crossoverRate || mum == dad) {
      return (mum, dad)
    }

    val crossoverPoint: Int = RandInt(0, chromosomeLength - 1)

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

  private def selectChromosomeByRoulette(): Genome = {
    //generate a random number between 0 & total fitness count
    val Slice: Double = RandFloat * totalFitness
    //this will be set to the chosen chromosome
    var TheChosenOne: Genome = null
    //go through the chromosomes adding up the fitness so far
    var FitnessSoFar: Double = 0
    var i: Int = 0
    var done = false
    while (i < popSize && !done) {
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

  private def applyEugenics(amount: Int, copies: Int): List[Genome] = {
    require(copies >= 0)
    require(amount >= 0)
    val best = population.slice(popSize - amount, popSize)
    List.fill(copies)(best).flatten
  }

  //	calculates the fittest and weakest genome and the average/total
  //	fitness scores
  private def calculateBestWorstAvTot() {
    totalFitness = 0
    var HighestSoFar: Double = 0
    var LowestSoFar: Double = 9999999
    for (i <- 0 until popSize) {
      if (population(i).fitness > HighestSoFar) {
        HighestSoFar = population(i).fitness
        fittestGenome = i
        bestFitness = HighestSoFar
      }

      if (population(i).fitness < LowestSoFar) {
        LowestSoFar = population(i).fitness
        worstFitness = LowestSoFar
      }
      totalFitness += population(i).fitness
    }
  }

  private def reset() {
    totalFitness = 0
    bestFitness = 0
    worstFitness = 9999999
  }

  def epoch(oldPopulation: List[Genome]): List[Genome] = {
    population = oldPopulation

    reset()

    population = population.sortBy(_.fitness)
    calculateBestWorstAvTot()

    var vecNewPop: List[Genome] = List.empty

    if ((CParams.iNumCopiesElite * CParams.iNumElite % 2) == 0) {
      vecNewPop = vecNewPop ++ applyEugenics(CParams.iNumElite, CParams.iNumCopiesElite)
    }

    while (vecNewPop.size < popSize) {
      val mum: Genome = selectChromosomeByRoulette()
      val dad: Genome = selectChromosomeByRoulette()

      val (baby1, baby2) = crossover(mum.weights, dad.weights)

      val baby1Mutated = mutate(baby1)
      val baby2Mutated = mutate(baby2)

      vecNewPop = vecNewPop :+ Genome(baby1Mutated, 0)
      vecNewPop = vecNewPop :+ Genome(baby2Mutated, 0)
    }

    population = vecNewPop
    population
  }

  def averageFitness: Double = totalFitness / popSize
}
