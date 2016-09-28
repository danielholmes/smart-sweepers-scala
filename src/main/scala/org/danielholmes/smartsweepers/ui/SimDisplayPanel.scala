package org.danielholmes.smartsweepers.ui

import java.awt.Color
import java.awt.geom.AffineTransform

import org.danielholmes.smartsweepers.original.CParams
import org.danielholmes.smartsweepers.sim.{MineSweeper, Simulation}

import scala.swing.{Graphics2D, Panel}

class SimDisplayPanel(private var _sim: Simulation) extends Panel {
  def sim_=(newSim: Simulation): Unit = {
    _sim = sim
    repaint()
  }

  def sim = _sim

  override def paint(g: Graphics2D): Unit = {
    super.paint(g)

    for (mine <- sim.mines) {
      g.setColor(Color.GREEN)
      g.drawRect((mine.x - CParams.dMineScale).toInt, (mine.y - CParams.dMineScale).toInt, (CParams.dMineScale * 2).toInt, (CParams.dMineScale * 2).toInt)
    }

    for (i <- sim.sweepers.indices) {
      if (i == CParams.iNumElite) g.setColor(Color.RED)
      else g.setColor(Color.BLACK)
      val s: MineSweeper = sim.sweepers(i)
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
  }
}
