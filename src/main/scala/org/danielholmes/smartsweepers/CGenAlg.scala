package org.danielholmes.smartsweepers

import java.util.Collections
import java.util
import org.danielholmes.smartsweepers.Utils.RandFloat
import org.danielholmes.smartsweepers.Utils.RandInt

class CGenAlg(// size of population
              var m_iPopSize: Int, //probability that a chromosomes bits will mutate.
              //Try figures around 0.05 to 0.3 ish
              var m_dMutationRate: Double, //probability of chromosomes crossing over bits
              //0.7 is pretty good
              var m_dCrossoverRate: Double, //amount of weights per chromo
              var m_iChromoLength: Int) {
  //total fitness of population
  private var m_dTotalFitness = 0.0
  //best fitness this population
  private var m_dBestFitness = 0.0
  //worst
  private var m_dWorstFitness = 99999999.0
  //average fitness
  private var m_dAverageFitness: Double = .0
  //this holds the entire population of chromosomes
  private var m_vecPop = new util.Vector[SGenome]
  //keeps track of the best genome
  private var m_iFittestGenome: Int = 0
  //generation counter
  private var m_cGeneration: Int = 0
  //initialise population with chromosomes consisting of random
  //weights and all fitnesses set to zero
  var i: Int = 0
  while (i < m_iPopSize) {
    {
      m_vecPop.add(new SGenome)
      var j: Int = 0
      while (j < m_iChromoLength) {
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

  private def Crossover(mum: util.Vector[Double], dad: util.Vector[Double], baby1: util.Vector[Double], baby2: util.Vector[Double]) {
    //just return parents as offspring dependent on the rate
    //or if parents are the same
    if ((RandFloat > m_dCrossoverRate) || (mum eq dad)) {
      baby1.addAll(mum)
      baby2.addAll(dad)
      return
    }
    //determine a crossover point
    val cp: Int = RandInt(0, m_iChromoLength - 1)
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

  private def Mutate(chromo: util.Vector[Double]) {
    //traverse the chromosome and mutate each weight dependent
    //on the mutation rate
    var i: Int = 0
    while (i < chromo.size) {
      {
        //do we perturb this weight?
        if (RandFloat < m_dMutationRate) {
          //add or subtract a small value to the weight
          chromo.set(i, chromo.get(i) + (Utils.RandomClamped * CParams.dMaxPerturbation))
        }
      }
      {
        i += 1; i
      }
    }
  }

  private def GetChromoRoulette: SGenome = {
    //generate a random number between 0 & total fitness count
    val Slice: Double = RandFloat * m_dTotalFitness
    //this will be set to the chosen chromosome
    var TheChosenOne: SGenome = null
    //go through the chromosomes adding up the fitness so far
    var FitnessSoFar: Double = 0
    var i: Int = 0
    var done = false
    while (i < m_iPopSize && !done) {
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
  private def GrabNBest(NBest: Int, NumCopies: Int, vecPop: util.Vector[SGenome]) {
    //add the required amount of copies of the n most fittest
    //to the supplied vector
    for (j <- 0 until NBest) {
      var i: Int = 0
      while (i < NumCopies) {
        {
          vecPop.add(m_vecPop.get((m_iPopSize - 1) - NBest))
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
    while (i < m_iPopSize) {
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
    m_dAverageFitness = m_dTotalFitness / m_iPopSize
  }

  //	resets all the relevant variables ready for a new generation
  private def Reset() {
    m_dTotalFitness = 0
    m_dBestFitness = 0
    m_dWorstFitness = 9999999
    m_dAverageFitness = 0
  }

  //this runs the GA for one generation.
  def Epoch(old_pop: util.Vector[SGenome]): util.Vector[SGenome] = {
    //assign the given population to the classes population
    m_vecPop = old_pop
    //reset the appropriate variables
    Reset()
    //sort the population (for scaling and elitism)
    Collections.sort(m_vecPop)
    //calculate best, worst, average and total fitness
    CalculateBestWorstAvTot()
    //create a temporary vector to store new chromosones
    val vecNewPop: util.Vector[SGenome] = new util.Vector[SGenome]
    //Now to add a little elitism we shall add in some copies of the
    //fittest genomes. Make sure we add an EVEN number or the roulette
    //wheel sampling will crash
    if ((CParams.iNumCopiesElite * CParams.iNumElite % 2) == 0) GrabNBest(CParams.iNumElite, CParams.iNumCopiesElite, vecNewPop)
    //now we enter the GA loop
    //repeat until a new population is generated
    while (vecNewPop.size < m_iPopSize) {
      //grab two chromosomes
      val mum: SGenome = GetChromoRoulette
      val dad: SGenome = GetChromoRoulette
      //create some offspring via crossover
      val baby1: util.Vector[Double] = new util.Vector[Double]
      val baby2: util.Vector[Double] = new util.Vector[Double]
      Crossover(mum.vecWeights, dad.vecWeights, baby1, baby2)
      //now we mutate
      Mutate(baby1)
      Mutate(baby2)
      //now copy into vecNewPop population
      vecNewPop.add(new SGenome(baby1, 0))
      vecNewPop.add(new SGenome(baby2, 0))
    }
    //finished so assign new pop back into m_vecPop
    m_vecPop = vecNewPop
    m_vecPop
  }

  def GetChromos: util.Vector[SGenome] = m_vecPop

  def AverageFitness: Double = m_dTotalFitness / m_iPopSize

  def BestFitness: Double = m_dBestFitness
}
