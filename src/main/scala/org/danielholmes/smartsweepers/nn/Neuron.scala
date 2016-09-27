package org.danielholmes.smartsweepers.nn

import org.danielholmes.smartsweepers.CParams
import scala.util.Random

case class Neuron(inputWeights: List[Double], biasWeight: Double) {
  require(inputWeights.nonEmpty)

  val numInputs = inputWeights.size
  val numberOfWeights = numInputs + 1

  lazy val allWeights = inputWeights :+ biasWeight

  def getActivation(inputs: List[Double]): Double = {
    require(inputs.size == inputWeights.size, s"inputs ${inputs.size} should = ${inputWeights.size}")
    sigmoid(getNetInput(inputs), CParams.dActivationResponse)
  }

  private def getNetInput(inputs: List[Double]): Double = {
    inputs.indices
      .map(i => inputWeights(i) * inputs(i))
      .toList
      .sum + (CParams.dBias * biasWeight)
  }

  private def sigmoid(netInput: Double, response: Double): Double = 1 / (1 + Math.exp(-netInput / response))
}

object Neuron {
  val randomiser = new Random

  def apply(numInputs: Int): Neuron = {
    Neuron(List.fill(numInputs) { randomWeight() }, randomWeight())
  }

  private def randomWeight(): Double = randomiser.nextDouble - randomiser.nextDouble
}