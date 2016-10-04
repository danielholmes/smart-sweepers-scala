package org.danielholmes.smartsweepers.ui

import java.awt.Color
import java.awt.geom.AffineTransform

import org.danielholmes.smartsweepers.sim.{Mine, MineSweeper, SimItem, Simulation}

import scala.swing.{Graphics2D, Panel}

class SimDisplayPanel(private var _sim: Simulation) extends Panel {
  private val NumberToHighlight = 4

  def sim_=(newSim: Simulation): Unit = {
    _sim = newSim
    repaint()
  }

  def sim = _sim

  override def paint(g: Graphics2D): Unit = {
    super.paint(g)

    renderItems(g, sim.items)
  }

  private def renderItems(g: Graphics2D, items: List[SimItem]): Unit = {
    items match {
      case Nil => Unit
      case Mine(_) :: xs => {
        renderMine(g, items.head.asInstanceOf[Mine])
        renderItems(g, xs)
      }
      case x :: xs => {
        renderMineSweeper(g, x.asInstanceOf[MineSweeper])
        renderItems(g, xs)
      }
    }
  }

  private def renderMine(g: Graphics2D, mine: Mine): Unit = {
    g.setColor(Color.GREEN)
    g.drawRect((mine.position.x - mine.size).toInt, (mine.position.y - mine.size).toInt, mine.size * 2, mine.size * 2)
  }

  private def renderMineSweeper(g: Graphics2D, s: MineSweeper): Unit = {
    //if (i <= NumberToHighlight) g.setColor(Color.RED)
    g.setColor(Color.BLACK)

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
