package org.danielholmes.smartsweepers.nn

import scala.annotation.tailrec

class NeuralNet(val layers: List[NeuronLayer], neuronFactory: NeuronFactory) {
  require(layers.nonEmpty)

  private val numInputs = layers.head.numInputs

  lazy val weights: List[Double] = layers.flatMap(_.neurons).flatMap(_.allWeights)

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
  def createRandom(neuronFactory: NeuronFactory, numOutputs: Int, neuronsPerHiddenLayer: Int, numHiddenLayers: Int, numInputs: Int): NeuralNet = {
    new NeuralNet(
      {
        if (numHiddenLayers > 0) {
          List(createRandomNeuronLayer(neuronFactory, neuronsPerHiddenLayer, numInputs)) ++
            List.fill(numHiddenLayers - 1) { createRandomNeuronLayer(neuronFactory, neuronsPerHiddenLayer, neuronsPerHiddenLayer) } ++
            List(createRandomNeuronLayer(neuronFactory, numOutputs, neuronsPerHiddenLayer))
        } else {
          List(createRandomNeuronLayer(neuronFactory, numOutputs, numInputs))
        }
      },
      neuronFactory
    )
  }

  private def createRandomNeuronLayer(neuronFactory: NeuronFactory, aNumNeurons: Int, aNumInputsPerNeuron: Int): NeuronLayer = {
    NeuronLayer(List.fill(aNumNeurons) { neuronFactory(aNumInputsPerNeuron) })
  }
}
