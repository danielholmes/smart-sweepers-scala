package org.danielholmes.smartsweepers.sim

import scala.annotation.tailrec

case class Simulation(private val size: Size, items: List[SimItem]) {
  lazy val mines = items.filter(_.isInstanceOf[Mine]).map(_.asInstanceOf[Mine])
  lazy val sweepers = items.filter(_.isInstanceOf[MineSweeper]).map(_.asInstanceOf[MineSweeper])
  lazy val rocks = items.filter(_.isInstanceOf[Rock]).map(_.asInstanceOf[Rock])

  def update(): Simulation = {
    val newItems = items.map(_.update(this)).map(constrainToBounds)
    val mines = newItems.filter(_.isInstanceOf[Mine]).map(_.asInstanceOf[Mine])
    val rocks = newItems.filter(_.isInstanceOf[Rock]).map(_.asInstanceOf[Rock])
    copy(
      items=hitRocks(
        hitMines(newItems, mines),
        rocks
      )
    )
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

  private def createRandomMine(): Mine = Mine(size.createRandomPosition())

  @tailrec
  private def hitRocks(newItems: List[SimItem], rocks: List[Rock]): List[SimItem] = {
    val sweepers = newItems.filter(_.isInstanceOf[MineSweeper]).map(_.asInstanceOf[MineSweeper])

    rocks match {
      case Nil => newItems
      case r :: rs => hitRocks(hitRock(newItems, r, sweepers), rs)
    }
  }

  @tailrec
  private def hitRock(newItems: List[SimItem], r: Rock, sweepers: List[MineSweeper]): List[SimItem] = {
    sweepers match {
      case Nil => newItems
      case s :: ss =>
        if (s.isHitting(r)) {
          val newNewItems = newItems.filter(i => i != s && i != r) :+ createRandomRock() :+ s.hitRock()
          assert(newNewItems.size == newItems.size)
          newNewItems
        } else {
          hitRock(newItems, r, ss)
        }
    }
  }

  private def createRandomRock(): Rock = Rock(size.createRandomPosition())

  private def constrainToBounds(i: SimItem): SimItem = i match {
    case s: MineSweeper => s.teleportTo(
      Vector2D(
        wrap(s.position.x, 0, size.width),
        wrap(s.position.y, 0, size.height)
      )
    )
    case _ => i
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
