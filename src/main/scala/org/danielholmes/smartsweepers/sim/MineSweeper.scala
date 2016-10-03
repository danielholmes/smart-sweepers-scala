package org.danielholmes.smartsweepers.sim

import org.danielholmes.smartsweepers.nn.NeuralNet
import org.danielholmes.smartsweepers.original.CParams

import scala.annotation.tailrec
import scala.math.{max, min}

class MineSweeper(val brain: NeuralNet, var position: Vector2D, var rotation: Double) {
  private var lookAt = Vector2D()
  private var speed = 0.0
  private var leftTrack = 0.16
  private var rightTrack = 0.16
  var numMinesCollected = 0
  private var closestMine = 0
  val size = 5

  def update(mines: List[Mine]): MineSweeper = {
    val vClosestMine: Vector2D = getClosestMine(mines).normalised

    val inputs = List(
      vClosestMine.x,
      vClosestMine.y,
      lookAt.x,
      lookAt.y
    )
    val output = brain.update(inputs)
    assert(output.size == 2, output.size + " outputs doesn't equal expected 2")

    leftTrack = output(0)
    rightTrack = output(1)

    var rotationForce = leftTrack - rightTrack
    rotationForce = max(min(rotationForce, CParams.dMaxTurnRate), -CParams.dMaxTurnRate)
    rotation += rotationForce
    speed = leftTrack + rightTrack

    lookAt = Vector2D(-Math.sin(rotation), Math.cos(rotation))

    position = position + (lookAt * speed)

    if (position.x > CParams.WindowWidth) position = Vector2D(0, position.y)
    if (position.x < 0) Vector2D(CParams.WindowWidth, position.y)
    if (position.y > CParams.WindowHeight) Vector2D(position.x, 0)
    if (position.y < 0) Vector2D(position.x, CParams.WindowHeight)

    this
  }

  private def getClosestMine(mines: List[Mine]): Vector2D = {
    var closest_so_far: Double = 99999
    var closestObject: Vector2D = Vector2D(0, 0)
    var i: Int = 0
    while (i < mines.size) {
      {
        val len_to_object: Double = (mines(i).position - position).length
        if (len_to_object < closest_so_far) {
          closest_so_far = len_to_object
          closestObject = position - mines(i).position
          closestMine = i
        }
      }
      {
        i += 1; i - 1
      }
    }
    closestObject
  }

  def checkForMine(mines: List[Mine]): Int = {
    @tailrec
    def checkForMine(i: Int, remaining: List[Mine]): Int = {
      remaining match {
        case Nil => -1
        case x :: xs => {
          val distanceToObject = position - x.position
          if (distanceToObject.length < (Mine.Size + size)) {
            i
          } else {
            checkForMine(i + 1, xs)
          }
        }
      }
    }

    checkForMine(0, mines)
  }

  def collectMine(): MineSweeper = {
    numMinesCollected += 1
    this
  }
}
