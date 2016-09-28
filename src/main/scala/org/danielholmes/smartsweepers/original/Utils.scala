package org.danielholmes.smartsweepers.original

import java.util.Random

@deprecated
object Utils {
  // Random number -1 to 1
  def RandomClamped: Double = RandFloat - RandFloat

  // Rand float 0 >= x < 1
  def RandFloat: Double = rand.nextDouble

  def Clamp(value: Double, min: Double, max: Double): Double = {
    var newValue = value
    if (value < min) newValue = min
    if (value > max) newValue = max
    newValue
  }

  private val rand: Random = new Random
}
