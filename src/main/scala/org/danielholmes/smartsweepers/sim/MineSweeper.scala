package org.danielholmes.smartsweepers.sim

import org.danielholmes.smartsweepers.original.Utils.{Clamp, RandFloat}
import org.danielholmes.smartsweepers.nn.NeuralNet
import org.danielholmes.smartsweepers.original.CParams

class MineSweeper(private var _brain: NeuralNet) {
  var position: Vector2D = Vector2D(RandFloat * CParams.WindowWidth, RandFloat * CParams.WindowHeight)
  private var lookAt: Vector2D = Vector2D()
  var rotation: Double = RandFloat * CParams.dTwoPi
  private var speed: Double = 0.0
  private var leftTrack: Double = 0.16
  private var rightTrack: Double = 0.16
  var fitness: Double = 0.0
  private var closestMine: Int = 0

  def reset() {
    position = Vector2D(RandFloat * CParams.WindowWidth, RandFloat * CParams.WindowHeight)
    fitness = 0
    rotation = RandFloat * CParams.dTwoPi
  }

  def brain = _brain

  def update(mines: List[Vector2D]): Unit = {
    val vClosestMine: Vector2D = GetClosestMine(mines).normalised

    val inputs = List(
      vClosestMine.x,
      vClosestMine.y,
      lookAt.x,
      lookAt.y
    )
    val output = _brain.update(inputs)
    assert(output.size == 2, output.size + " outputs doesn't equal expected 2")

    leftTrack = output(0)
    rightTrack = output(1)

    var RotForce: Double = leftTrack - rightTrack
    RotForce = Clamp(RotForce, -CParams.dMaxTurnRate, CParams.dMaxTurnRate)
    rotation += RotForce
    speed = leftTrack + rightTrack

    lookAt = Vector2D(-Math.sin(rotation), Math.cos(rotation))

    position = position + (lookAt * speed)

    if (position.x > CParams.WindowWidth) position = Vector2D(0, position.y)
    if (position.x < 0) Vector2D(CParams.WindowWidth, position.y)
    if (position.y > CParams.WindowHeight) Vector2D(position.x, 0)
    if (position.y < 0) Vector2D(position.x, CParams.WindowHeight)
  }

  private def GetClosestMine(mines: List[Vector2D]): Vector2D = {
    var closest_so_far: Double = 99999
    var closestObject: Vector2D = Vector2D(0, 0)
    var i: Int = 0
    while (i < mines.size) {
      {
        val len_to_object: Double = (mines(i) - position).length
        if (len_to_object < closest_so_far) {
          closest_so_far = len_to_object
          closestObject = position - mines(i)
          closestMine = i
        }
      }
      {
        i += 1; i - 1
      }
    }
    closestObject
  }

  def checkForMine(mines: List[Vector2D], size: Double): Int = {
    val DistToObject: Vector2D = position - mines(closestMine)
    if (DistToObject.length < (size + 5)) return closestMine
    -1
  }

  def incrementFitness() = fitness += 1

  def putWeights(w: List[Double]) = _brain = _brain.replaceWeights(w)
}
