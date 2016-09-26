package org.danielholmes.smartsweepers

import java.util.Random

object Utils {
  def RandomClamped: Double = RandFloat - RandFloat

  // Rand float 0 >= x < 1
  def RandFloat: Double = rand.nextDouble

  //returns a random integer between min and max
  def RandInt(x: Int, y: Int): Int = x + rand.nextInt(y - x)

  def Clamp(value: Double, min: Double, max: Double): Double = {
    var newValue = value
    if (value < min) newValue = min
    if (value > max) newValue = max
    newValue
  }

  private val rand: Random = new Random
}
