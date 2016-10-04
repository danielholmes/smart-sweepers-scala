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
      neuronsAndInputsPerLayer
        .map({ case (numLayerNeurons, numLayerInputs) => createRandomNeuronLayer(numLayerNeurons, numLayerInputs) })
    )
  }

  def createFromWeights(weights: List[Double]): NeuralNet = {
    var cWeight: Int = 0
    new NeuralNet(
      neuronsAndInputsPerLayer.map({
        case (numLayerNeurons, numLayerInputs) =>
          NeuronLayer(
            (0 until numLayerNeurons)
              .map(i => {
                val newInputWeights = weights.slice(cWeight, cWeight + numLayerInputs)
                cWeight += numLayerInputs
                val biasWeight = weights(cWeight)
                cWeight += 1

                neuronFactory(newInputWeights, biasWeight)
              })
              .toList
        )
      })
    )
  }

  private lazy val neuronsAndInputsPerLayer: List[(Int, Int)] =
    if (numHiddenLayers > 0) {
      List((neuronsPerHiddenLayer, numInputs)) ++
        List.fill(numHiddenLayers - 1) { (neuronsPerHiddenLayer, neuronsPerHiddenLayer) } ++
        List((numOutputs, neuronsPerHiddenLayer))
    } else {
      List((numOutputs, numInputs))
    }

  private def createRandomNeuronLayer(layerNumNeurons: Int, layerNumInputsPerNeuron: Int): NeuronLayer = {
    NeuronLayer(List.fill(layerNumNeurons) { neuronFactory(layerNumInputsPerNeuron) })
  }
}
