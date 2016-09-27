package org.danielholmes.smartsweepers.nn

case class NeuronLayer(neurons: List[Neuron]) {
  require(neurons.nonEmpty)

  val numNeurons = neurons.size
  val numInputs = neurons.head.numInputs

  def getOutputs(inputs: List[Double]): List[Double] = neurons.map(_.getActivation(inputs))
}
