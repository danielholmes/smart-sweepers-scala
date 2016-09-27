package org.danielholmes.smartsweepers

class Vector2D(var x: Double, var y: Double) {
  def this() {
    this(0.0, 0.0)
  }

  def *(rhs: Double): Vector2D = new Vector2D(x * rhs, y * rhs)

  def +(rhs: Vector2D): Vector2D = new Vector2D(x + rhs.x, y + rhs.y)

  def -(rhs: Vector2D): Vector2D = new Vector2D(x - rhs.x, y - rhs.y)

  lazy val length = Math.sqrt(x * x + y * y)

  // TODO: Investigate this. Doing in constructor seems to break GA
  def normalised: Vector2D  = new Vector2D(x / length, y / length)

  def dotProduct(other: Vector2D): Double = x * other.x + y * other.y

  def sign(other: Vector2D): Int = if (y * other.x > x * other.y) 1 else -1
}
