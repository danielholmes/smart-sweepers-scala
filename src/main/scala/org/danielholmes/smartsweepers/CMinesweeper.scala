package org.danielholmes.smartsweepers

import java.util

import org.danielholmes.smartsweepers.Utils.Clamp
import org.danielholmes.smartsweepers.Utils.RandFloat
import org.danielholmes.smartsweepers.nn.CNeuralNet

class CMinesweeper() {
  private val brain: CNeuralNet = new CNeuralNet
  private var position: Vector2D = new Vector2D(RandFloat * CParams.WindowWidth, RandFloat * CParams.WindowHeight)
  private var lookAt: Vector2D = new Vector2D()
  private var rotation: Double = RandFloat * CParams.dTwoPi
  private var speed: Double = 0.0
  private var leftTrack: Double = 0.16
  private var rightTrack: Double = 0.16
  private var fitness: Double = 0.0
  private var closestMine: Int = 0

  def reset() {
    position = new Vector2D(RandFloat * CParams.WindowWidth, RandFloat * CParams.WindowHeight)
    fitness = 0
    rotation = RandFloat * CParams.dTwoPi
  }

  def update(mines: util.List[Vector2D]): Boolean = {
    val vClosestMine: Vector2D = GetClosestMine(mines).normalised

    val inputs = List(
      vClosestMine.x,
      vClosestMine.y,
      lookAt.x,
      lookAt.y
    )
    val output = brain.update(inputs)
    //make sure there were no errors in calculating the output
    assert(output.size == CParams.iNumOutputs, output.size + " outputs doesn't equal expected " + CParams.iNumOutputs)
    //assign the outputs to the sweepers left & right tracks
    leftTrack = output(0)
    rightTrack = output(1)
    //calculate steering forces
    var RotForce: Double = leftTrack - rightTrack
    //clamp rotation
    RotForce = Clamp(RotForce, -CParams.dMaxTurnRate, CParams.dMaxTurnRate)
    rotation += RotForce
    speed = leftTrack + rightTrack

    lookAt = new Vector2D(-Math.sin(rotation), Math.cos(rotation))

    position = position + (lookAt * speed)
    //wrap around window limits
    if (position.x > CParams.WindowWidth) position = new Vector2D(0, position.y)
    if (position.x < 0) new Vector2D(CParams.WindowWidth, position.y)
    if (position.y > CParams.WindowHeight) new Vector2D(position.x, 0)
    if (position.y < 0) new Vector2D(position.x, CParams.WindowHeight)
    true
  }

  //	returns the vector from the sweeper to the closest mine
  private def GetClosestMine(mines: util.List[Vector2D]): Vector2D = {
    var closest_so_far: Double = 99999
    var vClosestObject: Vector2D = new Vector2D(0, 0)
    //cycle through mines to find closest
    var i: Int = 0
    while (i < mines.size) {
      {
        val len_to_object: Double = (mines.get(i) - position).length
        if (len_to_object < closest_so_far) {
          closest_so_far = len_to_object
          vClosestObject = position - mines.get(i)
          closestMine = i
        }
      }
      {
        i += 1; i - 1
      }
    }
    vClosestObject
  }

  //  this function checks for collision with its closest mine (calculated
  //  earlier and stored in m_iClosestMine)
  def CheckForMine(mines: util.List[Vector2D], size: Double): Int = {
    val DistToObject: Vector2D = position - mines.get(closestMine)
    if (DistToObject.length < (size + 5)) return closestMine
    -1
  }

  def Position: Vector2D = position

  def IncrementFitness() {
    fitness += 1
  }

  def Fitness: Double = fitness

  def Rotation: Double = rotation

  def PutWeights(w: util.List[Double]) {
    brain.putWeights(w)
  }

  def GetNumberOfWeights: Int = brain.numberOfWeights
}
