package org.danielholmes.smartsweepers.ga

import java.util
import java.util.Collections

import org.danielholmes.smartsweepers.Utils.{RandFloat, RandInt}
import org.danielholmes.smartsweepers.{CParams, Utils}

class CGenAlg(
               var popSize: Int,
               //Try figures around 0.05 to 0.3 ish
               var mutationRate: Double,
               var crossoverRate: Double,
               var chromosomeLength: Int
) {
  private var m_dTotalFitness = 0.0
  private var m_dBestFitness = 0.0
  private var m_dWorstFitness = 99999999.0
  private var m_dAverageFitness: Double = 0.0
  private var m_vecPop: util.List[Genome] = new util.ArrayList[Genome]
  private var m_iFittestGenome: Int = 0
  private var m_cGeneration: Int = 0

  var i: Int = 0
  while (i < popSize) {
    {
      m_vecPop.add(new Genome)
      var j: Int = 0
      while (j < chromosomeLength) {
        {
          m_vecPop.get(i).vecWeights.add(Utils.RandomClamped)
        }
        {
          j += 1; j
        }
      }
    }
    {
      i += 1; i
    }
  }

  private def crossover(mum: util.List[Double], dad: util.List[Double], baby1: util.List[Double], baby2: util.List[Double]) {
    //just return parents as offspring dependent on the rate
    //or if parents are the same
    if ((RandFloat > crossoverRate) || (mum eq dad)) {
      baby1.addAll(mum)
      baby2.addAll(dad)
      return
    }
    //determine a crossover point
    val cp: Int = RandInt(0, chromosomeLength - 1)
    //create the offspring
    var i: Int = 0
    for (i <- 0 until cp) {
      baby1.add(mum.get(i))
      baby2.add(dad.get(i))
    }
    for (i <- cp until mum.size) {
      baby1.add(dad.get(i))
      baby2.add(mum.get(i))
    }
  }

  private def mutate(chromo: util.List[Double]) {
    var i: Int = 0
    while (i < chromo.size) {
      {
        //do we perturb this weight?
        if (RandFloat < mutationRate) {
          //add or subtract a small value to the weight
          chromo.set(i, chromo.get(i) + (Utils.RandomClamped * CParams.dMaxPerturbation))
        }
      }
      {
        i += 1; i
      }
    }
  }

  private def selectChromosomeByRoulette(): Genome = {
    //generate a random number between 0 & total fitness count
    val Slice: Double = RandFloat * m_dTotalFitness
    //this will be set to the chosen chromosome
    var TheChosenOne: Genome = null
    //go through the chromosomes adding up the fitness so far
    var FitnessSoFar: Double = 0
    var i: Int = 0
    var done = false
    while (i < popSize && !done) {
      {
        FitnessSoFar += m_vecPop.get(i).dFitness
        //if the fitness so far > random number return the chromo at
        //this point
        if (FitnessSoFar >= Slice) {
          TheChosenOne = m_vecPop.get(i)
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
          vecPop.add(m_vecPop.get((popSize - 1) - NBest))
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
    m_dTotalFitness = 0
    var HighestSoFar: Double = 0
    var LowestSoFar: Double = 9999999
    var i: Int = 0
    while (i < popSize) {
      {
        //update fittest if necessary
        if (m_vecPop.get(i).dFitness > HighestSoFar) {
          HighestSoFar = m_vecPop.get(i).dFitness
          m_iFittestGenome = i
          m_dBestFitness = HighestSoFar
        }
        //update worst if necessary
        if (m_vecPop.get(i).dFitness < LowestSoFar) {
          LowestSoFar = m_vecPop.get(i).dFitness
          m_dWorstFitness = LowestSoFar
        }
        m_dTotalFitness += m_vecPop.get(i).dFitness
      } //next chromo
      {
        i += 1; i
      }
    }
    m_dAverageFitness = m_dTotalFitness / popSize
  }

  //	resets all the relevant variables ready for a new generation
  private def Reset() {
    m_dTotalFitness = 0
    m_dBestFitness = 0
    m_dWorstFitness = 9999999
    m_dAverageFitness = 0
  }

  //this runs the GA for one generation.
  def Epoch(old_pop: util.List[Genome]): util.List[Genome] = {
    //assign the given population to the classes population
    m_vecPop = old_pop
    //reset the appropriate variables
    Reset()
    //sort the population (for scaling and elitism)
    Collections.sort(m_vecPop)
    //calculate best, worst, average and total fitness
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
      crossover(mum.vecWeights, dad.vecWeights, baby1, baby2)
      //now we mutate
      mutate(baby1)
      mutate(baby2)
      //now copy into vecNewPop population
      vecNewPop.add(new Genome(baby1, 0))
      vecNewPop.add(new Genome(baby2, 0))
    }
    //finished so assign new pop back into m_vecPop
    m_vecPop = vecNewPop
    m_vecPop
  }

  def GetChromos: util.List[Genome] = m_vecPop

  def averageFitness: Double = m_dTotalFitness / popSize

  def bestFitness: Double = m_dBestFitness
}
