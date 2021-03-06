package org.danielholmes.smartsweepers.nn

case class Neuron(inputWeights: List[Double], biasWeight: Double, bias: Double, activationResponse: Double) {
  require(inputWeights.nonEmpty)
  require(activationResponse != 0)

  val numInputs = inputWeights.size
  val numberOfWeights = numInputs + 1

  lazy val allWeights = inputWeights :+ biasWeight

  def getActivation(inputs: List[Double]): Double = {
    require(inputs.size == inputWeights.size, s"inputs ${inputs.size} should = ${inputWeights.size}")
    sigmoid(getNetInput(inputs))
  }

  private def getNetInput(inputs: List[Double]): Double = {
    inputs.indices
      .map(i => inputWeights(i) * inputs(i))
      .toList
      .sum + (bias * biasWeight)
  }

  private def sigmoid(netInput: Double): Double = 1 / (1 + Math.exp(-netInput / activationResponse))
}
