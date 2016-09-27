package org.danielholmes.smartsweepers.nn

import scala.annotation.tailrec

class NeuralNet(val layers: List[NeuronLayer]) {
  require(layers.nonEmpty)

  def this(numOutputs: Int, neuronsPerHiddenLayer: Int, numHiddenLayers: Int, numInputs: Int) {
    this({
      if (numHiddenLayers > 0) {
        List(NeuronLayer(neuronsPerHiddenLayer, numInputs)) ++
          List.fill(numHiddenLayers - 1) { NeuronLayer(neuronsPerHiddenLayer, neuronsPerHiddenLayer) } ++
          List(NeuronLayer(numOutputs, neuronsPerHiddenLayer))
      } else {
        List(NeuronLayer(numOutputs, numInputs))
      }
    })
  }

  private val numInputs = layers.head.numInputs

  def weights(): List[Double] = layers.flatMap(_.neurons).flatMap(_.allWeights)

  def replaceWeights(weights: List[Double]): NeuralNet = {
    var cWeight: Int = 0
    new NeuralNet(
      layers.map(layer => {
        NeuronLayer(
          layer.neurons
            .map(neuron => {
              val newInputWeights = weights.slice(cWeight, cWeight + neuron.numInputs)
              cWeight += neuron.numInputs
              val biasWeight = weights(cWeight)
              cWeight += 1

              Neuron(newInputWeights, biasWeight)
            })
        )
      })
    )
  }

  def totalNumberOfWeights: Int = layers.flatMap(_.neurons).map(_.numberOfWeights).sum

  def update(inputs: List[Double]): List[Double] = {
    require(inputs.size == numInputs)

    @tailrec
    def update(remainingLayers: List[NeuronLayer], currentInputs: List[Double]): List[Double] = {
      remainingLayers match {
        case Nil => currentInputs
        case x :: xs => update(xs, x.getOutputs(currentInputs))
      }
    }

    update(layers, inputs)
  }
}
