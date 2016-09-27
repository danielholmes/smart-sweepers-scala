package org.danielholmes.smartsweepers.nn

import scala.util.Random

class NeuronFactory(private val bias: Double, private val activationResponse: Double) {
  private val randomiser = new Random

  def apply(inputWeights: List[Double], biasWeight: Double): Neuron = {
    Neuron(inputWeights, biasWeight, bias, activationResponse)
  }

  def apply(numInputs: Int): Neuron = {
    Neuron(List.fill(numInputs) { randomWeight }, randomWeight, bias, activationResponse)
  }

  private def randomWeight: Double = randomiser.nextDouble - randomiser.nextDouble
}
