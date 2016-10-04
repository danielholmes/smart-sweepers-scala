package org.danielholmes.smartsweepers.sim

import org.danielholmes.smartsweepers.nn.NeuralNet

import scala.math.{max, min}

case class MineSweeper private (
  brain: NeuralNet,
  position: Vector2D,
  rotation: Double,
  numMinesCollected: Int,
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
    val newPosition = position + (newLookAt * speed)

    MineSweeper(
      brain = brain,
      position = newPosition,
      rotation = newRotation,
      numMinesCollected = numMinesCollected,
      lookAt = newLookAt
    )
  }

  def moveTo(newPosition: Vector2D): MineSweeper = {
    if (newPosition == position) {
      this
    } else {
      MineSweeper(
        brain = brain,
        position = newPosition,
        rotation = rotation,
        numMinesCollected = numMinesCollected,
        lookAt = lookAt
      )
    }
  }

  private def getVectorToClosestMine(mines: List[Mine]): Vector2D = {
    require(mines.nonEmpty)
    mines.map(position - _.position).minBy(_.length)
  }

  def isHitting(mine: Mine): Boolean = {
    val distanceToObject = position - mine.position
    distanceToObject.length < (Mine.Size + size)
  }

  def collectMine(): MineSweeper = MineSweeper(
    brain = brain,
    position = position,
    rotation = rotation,
    numMinesCollected = numMinesCollected + 1,
    lookAt = lookAt
  )
}

object MineSweeper {
  val MaxTurnRate = 0.3

  def apply(brain: NeuralNet, position: Vector2D, rotation: Double): MineSweeper =
    MineSweeper(brain, position, rotation, 0, Vector2D())
}
