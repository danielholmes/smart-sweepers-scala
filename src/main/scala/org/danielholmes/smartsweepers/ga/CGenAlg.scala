package org.danielholmes.smartsweepers.ga

import java.util
import java.util.Collections

import org.danielholmes.smartsweepers.Utils.{RandFloat, RandInt}
import org.danielholmes.smartsweepers.{CParams, Utils}

import scala.collection.JavaConverters._

class CGenAlg(
               var popSize: Int,
               //Try figures around 0.05 to 0.3 ish
               var mutationRate: Double,
               var crossoverRate: Double,
               var chromosomeLength: Int
) {
  private var totalFitness = 0.0
  private var bestFitness = 0.0
  private var worstFitness = 99999999.0
  private var population: util.List[Genome] = new util.ArrayList[Genome]
  private var fittestGenome: Int = 0
  private var generation: Int = 0

  for (i <- 0 until popSize) {
    val weights = new util.ArrayList[Double]
    for (j <- 0 until chromosomeLength) {
      weights.add(Utils.RandomClamped)
    }
    population.add(Genome(weights.asScala.toList, 0))
  }

  private def crossover(mum: List[Double], dad: List[Double], baby1: util.List[Double], baby2: util.List[Double]) {
    //just return parents as offspring dependent on the rate
    //or if parents are the same
    if ((RandFloat > crossoverRate) || (mum eq dad)) {
      baby1.addAll(mum.asJava)
      baby2.addAll(dad.asJava)
      return
    }
    //determine a crossover point
    val cp: Int = RandInt(0, chromosomeLength - 1)
    //create the offspring
    var i: Int = 0
    for (i <- 0 until cp) {
      baby1.add(mum(i))
      baby2.add(dad(i))
    }
    for (i <- cp until mum.size) {
      baby1.add(dad(i))
      baby2.add(mum(i))
    }
  }

  private def mutate(chromo: util.List[Double]): List[Double] = {
    for (i <- 0 until chromo.size) {
      if (RandFloat < mutationRate) {
        chromo.set(i, chromo.get(i) + (Utils.RandomClamped * CParams.dMaxPerturbation))
      }
    }
    chromo.asScala.toList
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
        FitnessSoFar += population.get(i).fitness
        //if the fitness so far > random number return the chromo at
        //this point
        if (FitnessSoFar >= Slice) {
          TheChosenOne = population.get(i)
          done = true
        }
      }
      {
        i += 1; i
      }
    }
    TheChosenOne
  }

  //	This works like an advanced form of elitism by inserting NumCopies
  //  copies of the NBest most fittest genomes into a population vector
  private def GrabNBest(NBest: Int, NumCopies: Int, vecPop: util.List[Genome]) {
    //add the required amount of copies of the n most fittest
    //to the supplied vector
    for (j <- 0 until NBest) {
      var i: Int = 0
      while (i < NumCopies) {
        {
          vecPop.add(population.get((popSize - 1) - NBest))
        }
        {
          i += 1; i
        }
      }
    }
  }

  //	calculates the fittest and weakest genome and the average/total
  //	fitness scores
  private def CalculateBestWorstAvTot() {
    totalFitness = 0
    var HighestSoFar: Double = 0
    var LowestSoFar: Double = 9999999
    var i: Int = 0
    while (i < popSize) {
      {
        //update fittest if necessary
        if (population.get(i).fitness > HighestSoFar) {
          HighestSoFar = population.get(i).fitness
          fittestGenome = i
          bestFitness = HighestSoFar
        }
        //update worst if necessary
        if (population.get(i).fitness < LowestSoFar) {
          LowestSoFar = population.get(i).fitness
          worstFitness = LowestSoFar
        }
        totalFitness += population.get(i).fitness
      } //next chromo
      {
        i += 1; i
      }
    }
  }

  //	resets all the relevant variables ready for a new generation
  private def Reset() {
    totalFitness = 0
    bestFitness = 0
    worstFitness = 9999999
  }

  //this runs the GA for one generation.
  def Epoch(old_pop: util.List[Genome]): util.List[Genome] = {
    //assign the given population to the classes population
    population = old_pop
    //reset the appropriate variables
    Reset()

    population = population.asScala.toList.sortBy(_.fitness).asJava
    CalculateBestWorstAvTot()

    //create a temporary vector to store new chromosones
    val vecNewPop: util.List[Genome] = new util.ArrayList[Genome]
    //Now to add a little elitism we shall add in some copies of the
    //fittest genomes. Make sure we add an EVEN number or the roulette
    //wheel sampling will crash
    if ((CParams.iNumCopiesElite * CParams.iNumElite % 2) == 0) GrabNBest(CParams.iNumElite, CParams.iNumCopiesElite, vecNewPop)
    //now we enter the GA loop
    //repeat until a new population is generated
    while (vecNewPop.size < popSize) {
      //grab two chromosomes
      val mum: Genome = selectChromosomeByRoulette()
      val dad: Genome = selectChromosomeByRoulette()
      //create some offspring via crossover
      val baby1: util.List[Double] = new util.ArrayList[Double]
      val baby2: util.List[Double] = new util.ArrayList[Double]
      crossover(mum.weights, dad.weights, baby1, baby2)

      val baby1Mutated = mutate(baby1)
      val baby2Mutated = mutate(baby2)

      vecNewPop.add(Genome(baby1Mutated, 0))
      vecNewPop.add(Genome(baby2Mutated, 0))
    }
    //finished so assign new pop back into m_vecPop
    population = vecNewPop
    population
  }

  def GetChromos: util.List[Genome] = population

  def averageFitness: Double = totalFitness / popSize
}
