package org.danielholmes.smartsweepers.ui

import java.awt.Color
import java.awt.geom.AffineTransform

import org.danielholmes.smartsweepers.original.CParams
import org.danielholmes.smartsweepers.sim.{MineSweeper, Simulation}

import scala.swing.{Graphics2D, Panel}

class SimDisplayPanel(private var _sim: Simulation) extends Panel {
  def sim_=(newSim: Simulation): Unit = {
    _sim = newSim
    repaint()
  }

  def sim = _sim

  override def paint(g: Graphics2D): Unit = {
    super.paint(g)

    for (mine <- sim.mines) {
      g.setColor(Color.GREEN)
      g.drawRect((mine.position.x - mine.size).toInt, (mine.position.y - mine.size).toInt, mine.size * 2, mine.size * 2)
    }

    val orderedSweepers = sim.sweepers.sortBy(_.numMinesCollected).reverse
    for (i <- orderedSweepers.indices) {
      if (i <= CParams.iNumElite) g.setColor(Color.RED)
      else g.setColor(Color.BLACK)
      val s: MineSweeper = orderedSweepers(i)
      val oldTransform: AffineTransform = g.getTransform
      g.rotate(s.rotation, s.position.x, s.position.y)
      // Body
      g.drawRect((s.position.x - s.size).toInt, (s.position.y - s.size).toInt, s.size * 2, s.size * 2)
      // Left Track
      val trackWidth: Int = s.size / 2
      g.drawRect((s.position.x - s.size).toInt, (s.position.y - s.size).toInt, trackWidth, s.size * 2)
      // Right Track
      g.drawRect((s.position.x + s.size - trackWidth).toInt, (s.position.y - s.size).toInt, trackWidth, s.size * 2)
      // Nose
      val NOSE_SIZE: Int = s.size
      g.drawLine((s.position.x - s.size).toInt, (s.position.y + s.size).toInt, s.position.x.toInt, (s.position.y + s.size + NOSE_SIZE).toInt)
      g.drawLine(s.position.x.toInt, (s.position.y + s.size + NOSE_SIZE).toInt, (s.position.x + s.size).toInt, (s.position.y + s.size).toInt)
      g.setTransform(oldTransform)
    }
  }
}
