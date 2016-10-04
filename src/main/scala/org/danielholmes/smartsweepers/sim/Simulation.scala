package org.danielholmes.smartsweepers.sim

import scala.annotation.tailrec
import scala.util.Random

case class Simulation(private val size: Size, items: List[SimItem]) {
  def mines = items.filter(_.isInstanceOf[Mine]).map(_.asInstanceOf[Mine])
  def sweepers = items.filter(_.isInstanceOf[MineSweeper]).map(_.asInstanceOf[MineSweeper])

  def update(): Simulation = {
    val newItems = items.map(_.update(this)).map(constrainToBounds)
    val mines = newItems.filter(_.isInstanceOf[Mine]).map(_.asInstanceOf[Mine])
    Simulation(size, hitMines(newItems, mines))
  }

  @tailrec
  private def hitMines(newItems: List[SimItem], mines: List[Mine]): List[SimItem] = {
    val sweepers = newItems.filter(_.isInstanceOf[MineSweeper]).map(_.asInstanceOf[MineSweeper])

    mines match {
      case Nil => newItems
      case m :: ms => hitMines(hitMine(newItems, m, sweepers), ms)
    }
  }

  @tailrec
  private def hitMine(newItems: List[SimItem], m: Mine, sweepers: List[MineSweeper]): List[SimItem] = {
    sweepers match {
      case Nil => newItems
      case s :: ss =>
        if (s.isHitting(m)) {
          val newNewItems = newItems.filter(i => i != s && i != m) :+ createRandomMine() :+ s.collectMine()
          assert(newNewItems.size == newItems.size)
          newNewItems
        } else {
          hitMine(newItems, m, ss)
        }
    }
  }

  private def createRandomMine(): Mine = {
    Mine(Vector2D(Simulation.randomiser.nextDouble * size.width, Simulation.randomiser.nextDouble * size.height))
  }

  private def constrainToBounds(i: SimItem): SimItem = i match {
    case m: Mine => m
    case s: MineSweeper => s.moveTo(
      Vector2D(
        wrap(s.position.x, 0, size.width),
        wrap(s.position.y, 0, size.height)
      )
    )
  }

  private def wrap(x: Double, min: Double, max: Double): Double = {
    if (x < min) {
      max
    } else if (x > max) {
      min
    } else {
      x
    }
  }
}

object Simulation {
  private val randomiser = new Random
}
