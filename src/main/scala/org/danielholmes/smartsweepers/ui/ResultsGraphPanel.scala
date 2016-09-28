package org.danielholmes.smartsweepers.ui

import java.awt.Color

import org.danielholmes.smartsweepers.GenerationSummary

import scala.annotation.tailrec
import scala.swing.{Graphics2D, Panel}

class ResultsGraphPanel extends Panel {
  private var _results: List[GenerationSummary] = List.empty

  def results_=(results: List[GenerationSummary]): Unit = {
    if (results != _results) {
      _results = results
      repaint()
    }
  }

  def results = _results

  override def paint(g: Graphics2D): Unit = {
    g.setColor(Color.LIGHT_GRAY)
    g.drawRect(0, 0, size.width - 1, size.height - 1)

    if (results.size > 1) {
      val xSlice = size.width / (results.size - 1).toDouble
      val ySlice = size.height / results.map(_.maxFitness).max

      g.setColor(Color.LIGHT_GRAY)
      val GRID_SIZE = 10
      drawYGrid(g, 0, ySlice * GRID_SIZE)

      g.setColor(Color.BLUE)
      drawResults(g, 0, results.map(_.averageFitness), xSlice, ySlice)

      g.setColor(Color.RED)
      drawResults(g, 0, results.map(_.maxFitness), xSlice, ySlice)
    }
  }

  @tailrec
  private def drawYGrid(g: Graphics2D, currentY: Double, ySlice: Double): Unit = {
    if (currentY < size.height) {
      val lineY = (size.height - currentY).toInt
      g.drawLine(0, lineY, size.width, lineY)
      drawYGrid(g, currentY + ySlice, ySlice)
    }
  }

  @tailrec
  private def drawResults(g: Graphics2D, i: Int, values: List[Double], xSlice: Double, ySlice: Double): Unit = {
    values match {
      case Nil =>
      case y :: Nil =>
      case y :: ys => {
        g.drawLine(
          (i * xSlice).toInt,
          size.height - (y * ySlice).toInt,
          ((i + 1) * xSlice).toInt,
          size.height - (ys.head * ySlice).toInt
        )
        drawResults(g, i + 1, ys, xSlice, ySlice)
      }
    }
  }
}
