package org.danielholmes.smartsweepers

import java.awt.{Color, Graphics2D}
import java.awt.geom.AffineTransform

import org.danielholmes.smartsweepers.Utils.RandFloat
import org.danielholmes.smartsweepers.ga.{CGenAlg, Genome}

class CController() {
  private val cxClient: Int = CParams.WindowWidth
  private val cyClient: Int = CParams.WindowHeight
  private var m_vecThePopulation: List[Genome] = _
  private val m_vecSweepers: List[MineSweeper] = List.fill(CParams.iNumSweepers) { new MineSweeper }
  private var m_vecMines: List[Vector2D] = List.fill(CParams.iNumMines) { Vector2D(RandFloat * cxClient, RandFloat * cyClient) }
  private var ga: CGenAlg = _
  private var totalWeightsInNN: Int = 0
  private var averageFitness: List[Double] = List.empty
  private var bestFitness: List[Double] = List.empty
  private var m_bFastRender: Boolean = false
  private var m_iTicks: Int = 0
  private var m_iGenerations: Int = 0

  totalWeightsInNN = m_vecSweepers.head.numberOfWeights

  ga = new CGenAlg(m_vecSweepers.size, CParams.dMutationRate, CParams.dCrossoverRate, totalWeightsInNN)
  m_vecThePopulation = ga.population
  for (i <- m_vecThePopulation.indices) {
    m_vecSweepers(i).putWeights(m_vecThePopulation(i).weights)
  }

  def FastRender: Boolean = m_bFastRender

  def FastRender(arg: Boolean) = m_bFastRender = arg

  def FastRenderToggle() = m_bFastRender = !m_bFastRender

  def update: Boolean = {
    if ( {
      m_iTicks += 1; m_iTicks - 1
    } < CParams.iNumTicks) {
      m_vecThePopulation = m_vecSweepers.indices
        .map(i => {
          val g = m_vecThePopulation(i)
          val s = m_vecSweepers(i)
          if (!s.update(m_vecMines)) {
            throw new RuntimeException("Wrong amount of NN inputs!")
          }

          val GrabHit: Int = s.CheckForMine(m_vecMines, CParams.dMineScale)
          if (GrabHit >= 0) {
            s.incrementFitness()
            m_vecMines = m_vecMines.slice(0, GrabHit) ++
              List(Vector2D(RandFloat * cxClient, RandFloat * cyClient)) ++
              m_vecMines.slice(GrabHit + 1, m_vecMines.size)
          }

          Genome(g.weights, s.fitness)
        })
        .toList
    }
    else
    {
      averageFitness = averageFitness :+ ga.averageFitness
      bestFitness = bestFitness :+ ga.bestFitness

      m_iGenerations += 1
      m_iTicks = 0
      m_vecThePopulation = ga.epoch(m_vecThePopulation)

      for (i <- m_vecSweepers.indices) {
        val s = m_vecSweepers(i)
        s.putWeights(m_vecThePopulation(i).weights)
        s.reset()
      }
    }
    true
  }

  def render(g: Graphics2D) {
    g.setColor(Color.BLACK)
    g.drawString("Generation: " + m_iGenerations, 5, 15)
    //do not render if running at accelerated speed
    if (!m_bFastRender) {
      //render the mines
      for (mine <- m_vecMines) {
        g.setColor(Color.GREEN)
        g.drawRect((mine.x - CParams.dMineScale).toInt, (mine.y - CParams.dMineScale).toInt, (CParams.dMineScale * 2).toInt, (CParams.dMineScale * 2).toInt)
      }
      //render the sweepers
      for (i <- m_vecSweepers.indices) {
        if (i == CParams.iNumElite) g.setColor(Color.RED)
        else g.setColor(Color.BLACK)
        val s: MineSweeper = m_vecSweepers(i)
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
    g.drawString("Best Fitness Ever: " + bestFitness.reduceOption(_ max _).getOrElse(0), 5, 60)

    // Grid lines
    if (bestFitness.nonEmpty) {
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

  private def plotGraph(g: Graphics2D, color: Color, values: List[Double]) {
    val hSlice = cxClient / (m_iGenerations + 1)
    val vSlice = cyClient / ((ga.bestFitness + 1) * 2)
    g.setColor(color)
    for (i <- 1 until values.size) {
      val x = i * hSlice
      g.drawLine(x.toInt, (cyClient - vSlice * values(i - 1)).toInt, x + hSlice, (cyClient - vSlice * values(i)).toInt)
    }
  }
}
