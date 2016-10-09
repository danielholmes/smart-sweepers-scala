package org.danielholmes.smartsweepers.sim

import org.danielholmes.smartsweepers.nn.NeuralNet

import scala.math.{max, min}

case class MineSweeper private (
  brain: NeuralNet,
  position: Vector2D,
  rotation: Double,
  numMinesCollected: Int,
  numRocksHit: Int,
  distanceMoved: Double,
  private val lookAt: Vector2D
) extends SimItem {
  val size = 5

  override def update(sim: Simulation): SimItem = {
    val toClosestMine = getVectorToClosestPosition(sim.mines.map(_.position)).normalised
    val toClosestRock = getVectorToClosestPosition(sim.rocks.map(_.position)).normalised

    val inputs = List(
      toClosestMine.x,
      toClosestMine.y,
      toClosestRock.x,
      toClosestRock.y,
      lookAt.x,
      lookAt.y
    )
    assert(inputs.size == MineSweeper.NumBrainInputs)
    val output = brain.update(inputs)
    assert(output.size == 2, s"${output.size} outputs doesn't equal expected 2")

    val leftTrack = output(0)
    val rightTrack = output(1)

    val rotationForce = max(min(leftTrack - rightTrack, MineSweeper.MaxTurnRate), -MineSweeper.MaxTurnRate)
    val speed = leftTrack + rightTrack

    val newRotation = rotation + rotationForce
    val newLookAt = Vector2D(-Math.sin(newRotation), Math.cos(newRotation))
    val movement = newLookAt * speed
    val newPosition = position + movement

    copy(
      position = newPosition,
      rotation = newRotation,
      distanceMoved = distanceMoved + movement.length,
      lookAt = newLookAt
    )
  }

  def teleportTo(newPosition: Vector2D): MineSweeper = {
    if (newPosition == position) {
      this
    } else {
      copy(position = newPosition)
    }
  }

  // Might this be better if its the vector to touch rather than direct centre?
  private def getVectorToClosestPosition(positions: List[Vector2D]): Vector2D = {
    require(positions.nonEmpty)
    positions.map(position - _).minBy(_.length)
  }

  def isHitting(m: Mine): Boolean = isHitting(m.position, m.size)

  def isHitting(r: Rock): Boolean = isHitting(r.position, r.size)

  private def isHitting(targetPosition: Vector2D, targetSize: Int): Boolean = {
    val distanceToObject = targetPosition - position
    distanceToObject.length < (size + targetSize)
  }

  def collectMine(): MineSweeper = copy(numMinesCollected = numMinesCollected + 1)

  def hitRock(): MineSweeper = copy(numRocksHit = numRocksHit + 1)
}

object MineSweeper {
  val MaxTurnRate = 0.3
  val NumBrainInputs = 6

  def apply(brain: NeuralNet, position: Vector2D, rotation: Double): MineSweeper =
    MineSweeper(brain, position, rotation, 0, 0, 0, Vector2D())
}
