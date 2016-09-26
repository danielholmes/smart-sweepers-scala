package org.danielholmes.smartsweepers.nn

case class NeuronLayer(neurons: List[Neuron]) {
  val numNeurons = neurons.size

  def getOutputs(inputs: List[Double]): List[Double] = neurons.map(_.getActivation(inputs))
}

object NeuronLayer {
  def apply(aNumNeurons: Int, aNumInputsPerNeuron: Int): NeuronLayer = {
    NeuronLayer(List.fill(aNumNeurons) { Neuron(aNumInputsPerNeuron) })
  }
}
