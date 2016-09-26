package org.danielholmes.smartsweepers

import java.util
import org.danielholmes.smartsweepers.Utils.Clamp
import org.danielholmes.smartsweepers.Utils.RandFloat

class CMinesweeper() {
  m_ItsBrain = new CNeuralNet
  m_dRotation = RandFloat * CParams.dTwoPi
  m_lTrack = 0.16
  m_rTrack = 0.16
  m_dFitness = 0
  m_dScale = CParams.iSweeperScale
  m_iClosestMine = 0
  m_vLookAt = new SVector2D
  //create a random start position
  m_vPosition = new SVector2D(RandFloat * CParams.WindowWidth, RandFloat * CParams.WindowHeight)
  //the minesweeper's neural net
  private var m_ItsBrain: CNeuralNet = _
  //its position in the world
  private var m_vPosition: SVector2D = _
  //direction sweeper is facing
  private var m_vLookAt: SVector2D = _
  //its rotation (surprise surprise)
  private var m_dRotation: Double = .0
  private var m_dSpeed: Double = .0
  //to store output from the ANN
  private var m_lTrack: Double = .0
  private var m_rTrack: Double = .0
  //the sweeper's fitness score
  private var m_dFitness: Double = .0
  //the scale of the sweeper when drawn
  private var m_dScale: Double = .0
  //index position of closest mine
  private var m_iClosestMine: Int = 0

  //	Resets the sweepers position, fitness and rotation
  def Reset() {
    //reset the sweepers positions
    m_vPosition = new SVector2D(RandFloat * CParams.WindowWidth, RandFloat * CParams.WindowHeight)
    //and the fitness
    m_dFitness = 0
    //and the rotation
    m_dRotation = RandFloat * CParams.dTwoPi
  }

  //	First we take sensor readings and feed these into the sweepers brain.
  //
  //	The inputs are:
  //
  //	A vector to the closest mine (x, y)
  //	The sweepers 'look at' vector (x, y)
  //
  //	We receive two outputs from the brain.. lTrack & rTrack.
  //	So given a force for each track we calculate the resultant rotation
  //	and acceleration and apply to current velocity vector.

  def Update(mines: util.Vector[SVector2D]): Boolean = {
    //this will store all the inputs for the NN
    var inputs: util.Vector[Double] = new util.Vector[Double]
    //get vector to closest mine
    val vClosestMine: SVector2D = GetClosestMine(mines)
    //normalise it
    vClosestMine.Vec2DNormalize()
    //add in vector to closest mine
    inputs.add(vClosestMine.x)
    inputs.add(vClosestMine.y)
    //add in sweepers look at vector
    inputs.add(m_vLookAt.x)
    inputs.add(m_vLookAt.y)
    //update the brain and get feedback
    val output: util.Vector[Double] = m_ItsBrain.Update(inputs)
    //make sure there were no errors in calculating the output
    assert(output.size == CParams.iNumOutputs, output.size + " outputs doesn't equal expected " + CParams.iNumOutputs)
    //assign the outputs to the sweepers left & right tracks
    m_lTrack = output.get(0)
    m_rTrack = output.get(1)
    //calculate steering forces
    var RotForce: Double = m_lTrack - m_rTrack
    //clamp rotation
    RotForce = Clamp(RotForce, -CParams.dMaxTurnRate, CParams.dMaxTurnRate)
    m_dRotation += RotForce
    m_dSpeed = m_lTrack + m_rTrack
    //update Look At
    m_vLookAt.x = -Math.sin(m_dRotation)
    m_vLookAt.y = Math.cos(m_dRotation)
    //update position
    m_vPosition = m_vPosition.plus(m_vLookAt.times(m_dSpeed))
    //wrap around window limits
    if (m_vPosition.x > CParams.WindowWidth) m_vPosition.x = 0
    if (m_vPosition.x < 0) m_vPosition.x = CParams.WindowWidth
    if (m_vPosition.y > CParams.WindowHeight) m_vPosition.y = 0
    if (m_vPosition.y < 0) m_vPosition.y = CParams.WindowHeight
    true
  }

  //	returns the vector from the sweeper to the closest mine
  private def GetClosestMine(mines: util.Vector[SVector2D]): SVector2D = {
    var closest_so_far: Double = 99999
    var vClosestObject: SVector2D = new SVector2D(0, 0)
    //cycle through mines to find closest
    var i: Int = 0
    while (i < mines.size) {
      {
        val len_to_object: Double = mines.get(i).minus(m_vPosition).Vec2DLength
        if (len_to_object < closest_so_far) {
          closest_so_far = len_to_object
          vClosestObject = m_vPosition.minus(mines.get(i))
          m_iClosestMine = i
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
  def CheckForMine(mines: util.Vector[SVector2D], size: Double): Int = {
    val DistToObject: SVector2D = m_vPosition.minus(mines.get(m_iClosestMine))
    if (DistToObject.Vec2DLength < (size + 5)) return m_iClosestMine
    -1
  }

  def Position: SVector2D = m_vPosition

  def IncrementFitness() {
    m_dFitness += 1
  }

  def Fitness: Double = m_dFitness

  def Rotation: Double = m_dRotation

  def PutWeights(w: util.Vector[Double]) {
    m_ItsBrain.PutWeights(w)
  }

  def GetNumberOfWeights: Int = m_ItsBrain.GetNumberOfWeights
}
