package org.danielholmes.smartsweepers.nn

import org.scalatest._

class NeuralNetFactorySpec extends FlatSpec with Matchers {
  "NeuralNetFactory" should "construct the nn correctly" in {
    val n = new NeuralNetFactory(
      numInputs = 4,
      numOutputs = 2,
      numHiddenLayers = 1,
      neuronsPerHiddenLayer = 6,
      neuronFactory = new NeuronFactory(1, 1)
    ).createRandom()
    n.layers.size should be (2) // input/hidden + output
    n.layers(0).neurons.size should be (6)
    n.layers(0).neurons(0).inputWeights.size should be (4)
    n.layers(1).neurons.size should be (2)
    n.layers(1).neurons(0).inputWeights.size should be (6)
  }
}
