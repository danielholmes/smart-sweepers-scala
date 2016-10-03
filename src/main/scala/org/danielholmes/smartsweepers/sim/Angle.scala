package org.danielholmes.smartsweepers.sim

import scala.util.Random

object Angle {

  val Pi = math.Pi
  val TwoPi = 2 * Pi
  val PiOver2 = Pi / 2

  val Zero = Angle(0)
  val Quarter = Angle(Pi/2)
  val Half = Angle(Pi)
  val ThreeQuarters = Angle(3*Pi/2)
  val Full = Angle(TwoPi)

  implicit class DoubleOps(d: Double) {
    def degrees: Angle = Angle.degrees(d)
    def radians: Angle = Angle(d)
  }

  def degrees(degs: Double): Angle = Angle(degs * Pi / 180.0)

  def normalize(radians: Double): Double = {
    val fullCycles = (radians / TwoPi).asInstanceOf[Int]
    val possiblyNegative = radians - TwoPi * fullCycles

    if (possiblyNegative < 0) { possiblyNegative + TwoPi }
    else { possiblyNegative }
  }

  def atan2(y: Double, x: Double): Angle = {
    Angle(Math.atan2(y, x))
  }

  def apply(radians: Double): Angle = new Angle(normalize(radians))

  private val randomiser = new Random

  def createRandom(): Angle = apply(randomiser.nextDouble * TwoPi)
}

class Angle private (val radians: Double) extends AnyVal with Ordered[Angle] {
  import Angle.Pi

  def sin: Double = math.sin(radians)
  def cos: Double = math.cos(radians)
  def tan: Double = math.tan(radians)
  def opposite: Angle = Angle(radians + Pi)
  def degrees: Double = radians * 180.0 / Pi
  def unary_-(): Angle = Angle(-radians)
  def +(other: Angle): Angle = Angle(radians + other.radians)
  def -(other: Angle): Angle = Angle(radians - other.radians)
  def *(factor: Double): Angle = Angle(radians * factor)
  def /(factor: Double): Angle = Angle(radians / factor)
  def compare(a: Angle): Int = {
    if (this == a) { 0 }
    else if (this.radians < a.radians) { -1 }
    else { +1 }
  }

  private def shiftSin(x: Double) = math.sin(x - radians - Pi)

  def isLeftOf(a: Angle): Boolean =
    (a == opposite) || (a != this && shiftSin(a.radians) < 0)

  def isRightOf(a: Angle): Boolean =
    (a == opposite) || (a != this && shiftSin(a.radians) > 0)

  def distanceTo(a: Angle): Angle = {
    val diff = a - this
    if (diff < Angle.Half) diff else -diff
  }

  override def toString: String = s"Angle($radians)"
}

