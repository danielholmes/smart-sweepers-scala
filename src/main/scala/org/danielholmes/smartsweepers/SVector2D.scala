package org.danielholmes.smartsweepers

class SVector2D(var x: Double, var y: Double) {
  def this() {
    this(0.0, 0.0)
  }

  //overload the * operator
  def times(rhs: Double): SVector2D = new SVector2D(x * rhs, y * rhs)

  def plus(rhs: SVector2D): SVector2D = new SVector2D(x + rhs.x, y + rhs.y)

  def minus(rhs: Double): SVector2D = new SVector2D(x - rhs, y - rhs)

  def minus(rhs: SVector2D): SVector2D = new SVector2D(x - rhs.x, y - rhs.y)

  //	returns the length of a 2D vector
  def Vec2DLength: Double = Math.sqrt(x * x + y * y)

  //	normalizes a 2D Vector
  def Vec2DNormalize() {
    val vector_length: Double = Vec2DLength
    x = x / vector_length
    y = y / vector_length
  }

  //	calculates the dot product
  def Vec2DDot(other: SVector2D): Double = x * other.x + y * other.y

  //  returns positive if v2 is clockwise of v1, minus if anticlockwise
  def Vec2DSign(other: SVector2D): Int = if (y * other.x > x * other.y) 1
  else -1
}
