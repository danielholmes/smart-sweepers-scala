package org.danielholmes.smartsweepers

import java.awt._
import java.awt.geom.AffineTransform
import java.util

import org.danielholmes.smartsweepers.Utils.RandFloat
import org.danielholmes.smartsweepers.ga.{CGenAlg, Genome}

import scala.collection.JavaConverters._

class CController() {
  private var m_vecThePopulation: util.List[Genome] = _
  private val m_vecSweepers: util.List[CMinesweeper] = new util.ArrayList[CMinesweeper]
  private val m_vecMines: util.List[Vector2D] = new util.ArrayList[Vector2D]
  private var ga: CGenAlg = _
  private var m_NumWeightsInNN: Int = 0
  private val averageFitness: util.List[Double] = new util.ArrayList[Double]
  private val bestFitness: util.List[Double] = new util.ArrayList[Double]
  private var m_bFastRender: Boolean = false
  private var m_iTicks: Int = 0
  private var m_iGenerations: Int = 0
  private val cxClient: Int = CParams.WindowWidth
  private val cyClient: Int = CParams.WindowHeight
  private val m_NumSweepers = CParams.iNumSweepers
  private val m_NumMines = CParams.iNumMines

  for (i <- 0 until m_NumSweepers) {
    m_vecSweepers.add(new CMinesweeper)
  }
  m_NumWeightsInNN = m_vecSweepers.get(0).numberOfWeights

  ga = new CGenAlg(m_NumSweepers, CParams.dMutationRate, CParams.dCrossoverRate, m_NumWeightsInNN)
  m_vecThePopulation = ga.GetChromos
  for (i <- 0 until m_NumSweepers) {
    m_vecSweepers.get(i).putWeights(m_vecThePopulation.get(i).vecWeights)
  }

  for (i <- 0 until m_NumMines) {
    m_vecMines.add(Vector2D(RandFloat * cxClient, RandFloat * cyClient))
  }

  def FastRender: Boolean = m_bFastRender

  def FastRender(arg: Boolean) = m_bFastRender = arg

  def FastRenderToggle() = m_bFastRender = !m_bFastRender

  def update: Boolean = {
    if ( {
      m_iTicks += 1; m_iTicks - 1
    } < CParams.iNumTicks) {
      var i: Int = 0
      while (i < m_NumSweepers) {
        {
          //update the NN and position
          if (!m_vecSweepers.get(i).update(m_vecMines)) {
            //error in processing the neural net
            //MessageBox(m_hwndMain, "Wrong amount of NN inputs!", "Error", MB_OK);
            throw new RuntimeException("Wrong amount of NN inputs!")
            //return false;
          }
          //see if it's found a mine
          val GrabHit: Int = m_vecSweepers.get(i).CheckForMine(m_vecMines, CParams.dMineScale)
          if (GrabHit >= 0) {
            //we have discovered a mine so increase fitness
            m_vecSweepers.get(i).incrementFitness()
            //mine found so replace the mine with another at a random
            //position
            m_vecMines.set(GrabHit, Vector2D(RandFloat * cxClient, RandFloat * cyClient))
          }
          //update the chromos fitness score
          m_vecThePopulation.get(i).dFitness = m_vecSweepers.get(i).fitness
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
      averageFitness.add(ga.averageFitness)
      bestFitness.add(ga.bestFitness)
      //increment the generation counter
      m_iGenerations += 1
      //reset cycles
      m_iTicks = 0
      //run the GA to create a new population
      m_vecThePopulation = ga.Epoch(m_vecThePopulation)
      //insert the new (hopefully)improved brains back into the sweepers
      //and reset their positions etc
      var i: Int = 0
      while (i < m_NumSweepers) {
        {
          m_vecSweepers.get(i).putWeights(m_vecThePopulation.get(i).vecWeights)
          m_vecSweepers.get(i).reset()
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
        g.rotate(s.rotation, s.position.x, s.position.y)
        // Body
        g.drawRect((s.position.x - CParams.iSweeperScale).toInt, (s.position.y - CParams.iSweeperScale).toInt, CParams.iSweeperScale * 2, CParams.iSweeperScale * 2)
        // Left Track
        val trackWidth: Int = CParams.iSweeperScale / 2
        g.drawRect((s.position.x - CParams.iSweeperScale).toInt, (s.position.y - CParams.iSweeperScale).toInt, trackWidth, CParams.iSweeperScale * 2)
        // Right Track
        g.drawRect((s.position.x + CParams.iSweeperScale - trackWidth).toInt, (s.position.y - CParams.iSweeperScale).toInt, trackWidth, CParams.iSweeperScale * 2)
        // Nose
        val NOSE_SIZE: Int = CParams.iSweeperScale
        g.drawLine((s.position.x - CParams.iSweeperScale).toInt, (s.position.y + CParams.iSweeperScale).toInt, s.position.x.toInt, (s.position.y + CParams.iSweeperScale + NOSE_SIZE).toInt)
        g.drawLine(s.position.x.toInt, (s.position.y + CParams.iSweeperScale + NOSE_SIZE).toInt, (s.position.x + CParams.iSweeperScale).toInt, (s.position.y + CParams.iSweeperScale).toInt)
        g.setTransform(oldTransform)
      }
    } //end if
    else PlotStats(g)
  }

  //  Given a surface to draw on this function displays stats and a crude
  //  graph showing best and average fitness
  private def PlotStats(g: Graphics2D) {
    g.setColor(Color.BLACK)
    g.drawString("Best Fitness:      " + ga.bestFitness, 5, 30)
    g.drawString("Average Fitness:   " + ga.averageFitness, 5, 45)
    g.drawString("Best Fitness Ever: " + bestFitness.asScala.reduceOption(_ max _).getOrElse(0), 5, 60)

    // Grid lines
    if (bestFitness.asScala.nonEmpty) {
      val vSlice = cyClient / ((ga.bestFitness + 1) * 2)
      val gridSlice = vSlice * 10
      val numLines = Math.floor(cyClient / gridSlice).toInt
      g.setColor(Color.GRAY)
      for (y <- 1 to numLines) {
        val lineY = (cyClient - y * gridSlice).toInt
        g.drawLine(0, lineY, cxClient, lineY)
      }
    }

    plotGraph(g, Color.RED, bestFitness)
    plotGraph(g, Color.BLUE, averageFitness)
  }

  private def plotGraph(g: Graphics2D, color: Color, values: util.List[Double]) {
    val hSlice = cxClient / (m_iGenerations + 1)
    val vSlice = cyClient / ((ga.bestFitness + 1) * 2)
    g.setColor(color)
    for (i <- 1 until values.size) {
      val x = i * hSlice
      g.drawLine(x.toInt, (cyClient - vSlice * values.get(i - 1)).toInt, x + hSlice, (cyClient - vSlice * values.get(i)).toInt)
    }
  }
}
