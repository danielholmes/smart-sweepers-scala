package org.danielholmes.smartsweepers.nn

import org.scalatest._

class NeuronLayerSpec extends FlatSpec with Matchers {
  "NeuronLayer" should "return correct outputs" in {
    NeuronLayer(
      List(
        Neuron(List(0), 0, -1, 1),
        Neuron(List(0), 0, -1, 1)
      )
    ).getOutputs(List(1)) should be (List(0.5, 0.5))
  }

  it should "return correct outputs for multiple inputs" in {
    NeuronLayer(
      List(
        Neuron(List(0, 0), 0, -1, 1),
        Neuron(List(0, 0), 0, -1, 1)
      )
    ).getOutputs(List(1, 1)) should be (List(0.5, 0.5))
  }
}
