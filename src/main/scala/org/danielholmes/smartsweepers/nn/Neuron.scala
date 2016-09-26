package org.danielholmes.smartsweepers.nn

import org.danielholmes.smartsweepers.CParams
import scala.util.Random

case class Neuron(inputWeights: List[Double]) {
  val numInputs = inputWeights.size
  // TODO: Remove from "regular" weights
  val biasWeight = inputWeights.last

  def getActivation(inputs: List[Double]): Double = {
    require(inputs.size == inputWeights.size - 1, s"inputs ${inputs.size} should = ${inputWeights.size}")
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
  def apply(numInputs: Int): Neuron = {
    Neuron(List.fill(numInputs + 1) { new Random().nextDouble - new Random().nextDouble })
  }
}