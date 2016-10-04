package org.danielholmes.smartsweepers.nn

class NeuralNetFactory(
  private val neuronFactory: NeuronFactory,
  private val numOutputs: Int,
  private val neuronsPerHiddenLayer: Int,
  private val numHiddenLayers: Int,
  private val numInputs: Int
) {
  def createRandom(): NeuralNet = {
    new NeuralNet(
      {
        if (numHiddenLayers > 0) {
          List(createRandomNeuronLayer(neuronsPerHiddenLayer, numInputs)) ++
            List.fill(numHiddenLayers - 1) { createRandomNeuronLayer(neuronsPerHiddenLayer, neuronsPerHiddenLayer) } ++
            List(createRandomNeuronLayer(numOutputs, neuronsPerHiddenLayer))
        } else {
          List(createRandomNeuronLayer(numOutputs, numInputs))
        }
      }
    )
  }

  def createFromWeights(weights: List[Double]): NeuralNet = {
    var cWeight: Int = 0
    // TODO: Better way of doing this - e.g. maybe a List of layer specs (tuple in,out), createRandom also use
    val reference = createRandom()
    new NeuralNet(
      reference.layers.map(layer => {
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
      })
    )
  }

  private def createRandomNeuronLayer(layerNumNeurons: Int, layerNumInputsPerNeuron: Int): NeuronLayer = {
    NeuronLayer(List.fill(layerNumNeurons) { neuronFactory(layerNumInputsPerNeuron) })
  }
}
