package org.danielholmes.smartsweepers.nn

import scala.annotation.tailrec

class NeuralNet(val layers: List[NeuronLayer], neuronFactory: NeuronFactory) {
  require(layers.nonEmpty)

  private val numInputs = layers.head.numInputs

  lazy val weights: List[Double] = layers.flatMap(_.neurons).flatMap(_.allWeights)

  @deprecated("See NeuralNetFactory")
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

              neuronFactory(newInputWeights, biasWeight)
            })
        )
      }),
      neuronFactory
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

object NeuralNet {
  @deprecated("Use NeuralNetworkFactory instead")
  def createRandom(neuronFactory: NeuronFactory, numOutputs: Int, neuronsPerHiddenLayer: Int, numHiddenLayers: Int, numInputs: Int): NeuralNet = {
    val factory = new NeuralNetFactory(
      neuronFactory=neuronFactory,
      numOutputs=numOutputs,
      neuronsPerHiddenLayer=neuronsPerHiddenLayer,
      numHiddenLayers=numHiddenLayers,
      numInputs=numInputs
    )
    factory.createRandom()
  }
}
