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
    val toClosestMine = getVectorToClosestMine(sim.mines).normalised

    val inputs = List(
      toClosestMine.x,
      toClosestMine.y,
      lookAt.x,
      lookAt.y
    )
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

    MineSweeper(
      brain = brain,
      position = newPosition,
      rotation = newRotation,
      numMinesCollected = numMinesCollected,
      numRocksHit = numRocksHit,
      distanceMoved = distanceMoved + movement.length,
      lookAt = newLookAt
    )
  }

  def teleportTo(newPosition: Vector2D): MineSweeper = {
    if (newPosition == position) {
      this
    } else {
      MineSweeper(
        brain = brain,
        position = newPosition,
        rotation = rotation,
        numMinesCollected = numMinesCollected,
        numRocksHit = numRocksHit,
        distanceMoved = distanceMoved,
        lookAt = lookAt
      )
    }
  }

  private def getVectorToClosestMine(mines: List[Mine]): Vector2D = {
    require(mines.nonEmpty)
    mines.map(position - _.position).minBy(_.length)
  }

  def isHitting(m: Mine): Boolean = isHitting(m.position, m.size)

  def isHitting(r: Rock): Boolean = isHitting(r.position, r.size)

  private def isHitting(targetPosition: Vector2D, targetSize: Int): Boolean = {
    val distanceToObject = targetPosition - position
    distanceToObject.length < (size + targetSize)
  }

  def collectMine(): MineSweeper = MineSweeper(
    brain = brain,
    position = position,
    rotation = rotation,
    numMinesCollected = numMinesCollected + 1,
    numRocksHit = numRocksHit,
    distanceMoved = distanceMoved,
    lookAt = lookAt
  )

  def hitRock(): MineSweeper = MineSweeper(
    brain = brain,
    position = position,
    rotation = rotation,
    numMinesCollected = numMinesCollected,
    numRocksHit = numRocksHit + 1,
    distanceMoved = distanceMoved,
    lookAt = lookAt
  )
}

object MineSweeper {
  val MaxTurnRate = 0.3

  def apply(brain: NeuralNet, position: Vector2D, rotation: Double): MineSweeper =
    MineSweeper(brain, position, rotation, 0, 0, 0, Vector2D())
}
