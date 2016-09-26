package org.danielholmes.smartsweepers

import java.awt._
import java.awt.geom.AffineTransform
import java.util
import org.danielholmes.smartsweepers.Utils.RandFloat

//---------------------------------------constructor---------------------
//
//	initilaize the sweepers, their brains and the GA factory
//-----------------------------------------------------------------------
class CController() {
  //storage for the population of genomes
  private var m_vecThePopulation: util.Vector[SGenome] = _
  //and the minesweepers
  private var m_vecSweepers: util.Vector[CMinesweeper] = new util.Vector[CMinesweeper]
  //and the mines
  private var m_vecMines: util.Vector[SVector2D] = new util.Vector[SVector2D]
  //pointer to the GA
  private var m_pGA: CGenAlg = _
  private var m_NumWeightsInNN: Int = 0
  //stores the average fitness per generation for use
  //in graphing.
  private var m_vecAvFitness: util.Vector[Double] = new util.Vector[Double]
  //stores the best fitness per generation
  private var m_vecBestFitness: util.Vector[Double] = new util.Vector[Double]
  //toggles the speed at which the simulation runs
  private var m_bFastRender: Boolean = false
  //cycles per generation
  private var m_iTicks: Int = 0
  //generation counter
  private var m_iGenerations: Int = 0
  //window dimensions
  private val cxClient: Int = CParams.WindowWidth
  private val cyClient: Int = CParams.WindowHeight
  private var m_NumSweepers = CParams.iNumSweepers
  private var m_NumMines = CParams.iNumMines

  for (i <- 0 until m_NumSweepers) {
    m_vecSweepers.add(new CMinesweeper)
  }
  //get the total number of weights used in the sweepers
  //NN so we can initialise the GA
  m_NumWeightsInNN = m_vecSweepers.get(0).GetNumberOfWeights
  //initialize the Genetic Algorithm class
  m_pGA = new CGenAlg(m_NumSweepers, CParams.dMutationRate, CParams.dCrossoverRate, m_NumWeightsInNN)
  //Get the weights from the GA and insert into the sweepers brains
  m_vecThePopulation = m_pGA.GetChromos
  for (i <- 0 until m_NumSweepers) {
    m_vecSweepers.get(i).PutWeights(m_vecThePopulation.get(i).vecWeights)
  }
  //initialize mines in random positions within the application window
  for (i <- 0 until m_NumMines) {
    m_vecMines.add(new SVector2D(RandFloat * cxClient, RandFloat * cyClient))
  }

  //accessor methods
  def FastRender: Boolean = m_bFastRender

  def FastRender(arg: Boolean) {
    m_bFastRender = arg
  }

  def FastRenderToggle() {
    m_bFastRender = !m_bFastRender
  }

  //	This is the main workhorse. The entire simulation is controlled from here.
  //	The comments should explain what is going on adequately.
  def Update: Boolean = {
    //run the sweepers through CParams.iNumTicks amount of cycles. During
    //this loop each sweepers NN is constantly updated with the appropriate
    //information from its surroundings. The output from the NN is obtained
    //and the sweeper is moved. If it encounters a mine its fitness is
    //updated appropriately,
    if ( {
      m_iTicks += 1; m_iTicks - 1
    } < CParams.iNumTicks) {
      var i: Int = 0
      while (i < m_NumSweepers) {
        {
          //update the NN and position
          if (!m_vecSweepers.get(i).Update(m_vecMines)) {
            //error in processing the neural net
            //MessageBox(m_hwndMain, "Wrong amount of NN inputs!", "Error", MB_OK);
            throw new RuntimeException("Wrong amount of NN inputs!")
            //return false;
          }
          //see if it's found a mine
          val GrabHit: Int = m_vecSweepers.get(i).CheckForMine(m_vecMines, CParams.dMineScale)
          if (GrabHit >= 0) {
            //we have discovered a mine so increase fitness
            m_vecSweepers.get(i).IncrementFitness()
            //mine found so replace the mine with another at a random
            //position
            m_vecMines.set(GrabHit, new SVector2D(RandFloat * cxClient, RandFloat * cyClient))
          }
          //update the chromos fitness score
          m_vecThePopulation.get(i).dFitness = m_vecSweepers.get(i).Fitness
        }
        {
          i += 1; i
        }
      }
    }
    else //Another generation has been completed.
    //Time to run the GA and update the sweepers with their new NNs
    {
      //update the stats to be used in our stat window
      m_vecAvFitness.add(m_pGA.AverageFitness)
      m_vecBestFitness.add(m_pGA.BestFitness)
      //increment the generation counter
      m_iGenerations += 1
      //reset cycles
      m_iTicks = 0
      //run the GA to create a new population
      m_vecThePopulation = m_pGA.Epoch(m_vecThePopulation)
      //insert the new (hopefully)improved brains back into the sweepers
      //and reset their positions etc
      var i: Int = 0
      while (i < m_NumSweepers) {
        {
          m_vecSweepers.get(i).PutWeights(m_vecThePopulation.get(i).vecWeights)
          m_vecSweepers.get(i).Reset()
        }
        {
          i += 1; i
        }
      }
    }
    true
  }

  def Render(g: Graphics2D) {
    g.setColor(Color.BLACK)
    g.drawString("Generation: " + m_iGenerations, 5, 15)
    //do not render if running at accelerated speed
    if (!m_bFastRender) {
      //render the mines
      for (i <- 0 until m_NumMines) {
        m_vecMines.get(i)
        g.setColor(Color.GREEN)
        g.drawRect((m_vecMines.get(i).x - CParams.dMineScale).toInt, (m_vecMines.get(i).y - CParams.dMineScale).toInt, (CParams.dMineScale * 2).toInt, (CParams.dMineScale * 2).toInt)
      }
      //render the sweepers
      for (i <- 0 until m_NumSweepers) {
        if (i == CParams.iNumElite) g.setColor(Color.RED)
        else g.setColor(Color.BLACK)
        val s: CMinesweeper = m_vecSweepers.get(i)
        val oldTransform: AffineTransform = g.getTransform
        g.rotate(s.Rotation, s.Position.x, s.Position.y)
        // Body
        g.drawRect((s.Position.x - CParams.iSweeperScale).toInt, (s.Position.y - CParams.iSweeperScale).toInt, CParams.iSweeperScale * 2, CParams.iSweeperScale * 2)
        // Left Track
        val trackWidth: Int = CParams.iSweeperScale / 2
        g.drawRect((s.Position.x - CParams.iSweeperScale).toInt, (s.Position.y - CParams.iSweeperScale).toInt, trackWidth, CParams.iSweeperScale * 2)
        // Right Track
        g.drawRect((s.Position.x + CParams.iSweeperScale - trackWidth).toInt, (s.Position.y - CParams.iSweeperScale).toInt, trackWidth, CParams.iSweeperScale * 2)
        // Nose
        val NOSE_SIZE: Int = CParams.iSweeperScale
        g.drawLine((s.Position.x - CParams.iSweeperScale).toInt, (s.Position.y + CParams.iSweeperScale).toInt, s.Position.x.toInt, (s.Position.y + CParams.iSweeperScale + NOSE_SIZE).toInt)
        g.drawLine(s.Position.x.toInt, (s.Position.y + CParams.iSweeperScale + NOSE_SIZE).toInt, (s.Position.x + CParams.iSweeperScale).toInt, (s.Position.y + CParams.iSweeperScale).toInt)
        g.setTransform(oldTransform)
      }
    } //end if
    else PlotStats(g)
  }

  //  Given a surface to draw on this function displays stats and a crude
  //  graph showing best and average fitness
  private def PlotStats(g: Graphics2D) {
    g.setColor(Color.BLACK)
    g.drawString("Best Fitness:    " + m_pGA.BestFitness, 5, 30)
    g.drawString("Average Fitness: " + m_pGA.AverageFitness, 5, 45)
    plotGraph(g, Color.RED, m_vecBestFitness)
    plotGraph(g, Color.BLUE, m_vecAvFitness)
  }

  private def plotGraph(g: Graphics2D, color: Color, values: util.Vector[Double]) {
    //render the graph
    val HSlice: Double = cxClient.toFloat / (m_iGenerations + 1)
    val VSlice: Double = cyClient.toFloat / ((m_pGA.BestFitness + 1) * 2)
    //plot the graph for the best fitness
    var x: Double = 0
    g.setColor(color)
    var i: Int = 1
    while (i < values.size) {
      {
        g.drawLine(x.toInt, (cyClient - VSlice * values.get(i - 1)).toInt, (x + HSlice).toInt, (cyClient - VSlice * values.get(i)).toInt)
        x += HSlice
      }
      {
        i += 1; i
      }
    }
  }
}
