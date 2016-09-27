package org.danielholmes.smartsweepers

case class Vector2D(x: Double, y: Double) {
  def *(rhs: Double): Vector2D = Vector2D(x * rhs, y * rhs)

  def +(rhs: Vector2D): Vector2D = Vector2D(x + rhs.x, y + rhs.y)

  def -(rhs: Vector2D): Vector2D = Vector2D(x - rhs.x, y - rhs.y)

  lazy val length = Math.sqrt(x * x + y * y)

  // TODO: Investigate this. Doing in constructor seems to break GA
  def normalised: Vector2D  = Vector2D(x / length, y / length)

  def dotProduct(other: Vector2D): Double = x * other.x + y * other.y

  def sign(other: Vector2D): Int = if (y * other.x > x * other.y) 1 else -1
}

object Vector2D {
  val Null = Vector2D(0, 0)

  def apply(): Vector2D = Null
}
