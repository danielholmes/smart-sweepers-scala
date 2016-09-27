package org.danielholmes.nnmine.nn

import org.danielholmes.smartsweepers.nn.{NeuralNet, Neuron, NeuronFactory, NeuronLayer}
import org.scalatest._

class NeuralNetSpec extends FlatSpec with Matchers {
  "NeuralNet" should "throw argument error when updating with wrong number of inputs" in {
    val n = NeuralNet.createRandom(
      numInputs=4,
      numOutputs=2,
      numHiddenLayers=1,
      neuronsPerHiddenLayer=5,
      neuronFactory=new NeuronFactory(10.0, 11.0)
    )

    an [IllegalArgumentException] should be thrownBy {
      n.update(List(1.0, 2.0))
    }
  }

  it should "run update on small set correctly" in {
    val n = NeuralNet.createRandom(
      numInputs=4,
      numOutputs=2,
      numHiddenLayers=1,
      neuronsPerHiddenLayer=5,
      neuronFactory=new NeuronFactory(10.0, 11.0)
    )
    val out = n.update(List(1, 2, 3, 4))
    out.size should be (2)
  }

  it should "return correct weights" in {
    val n = new NeuralNet(
      layers=List(
        NeuronLayer(List(
          Neuron(List(0.1, 0.2), 1.0, 10.0, 11.0),
          Neuron(List(0.3, 0.4), 2.0, 10.0, 11.0)
        )),
        NeuronLayer(List(
          Neuron(List(0.5, 0.6), 3.0, 10.0, 11.0),
          Neuron(List(0.7, 0.8), 4.0, 10.0, 11.0)
        ))
      ),
      neuronFactory=new NeuronFactory(10.0, 11.0)
    )
    n.weights should be (List(
      0.1, 0.2, 1.0, 0.3, 0.4, 2.0,
      0.5, 0.6, 3.0, 0.7, 0.8, 4.0
    ))
  }

  it should "replace weights correctly" in {
    val n = new NeuralNet(
      layers=List(
        NeuronLayer(List(
          Neuron(List(0.1, 0.2), 1.0, 10.0, 11.0),
          Neuron(List(0.3, 0.4), 2.0, 10.0, 11.0)
        )),
        NeuronLayer(List(
          Neuron(List(0.5, 0.6), 3.0, 10.0, 11.0),
          Neuron(List(0.7, 0.8), 4.0, 10.0, 11.0)
        ))
      ),
      neuronFactory=new NeuronFactory(10.0, 11.0)
    )
    val n2 = n.replaceWeights(List(
      1.1, 1.2, 2.0, 2.3, 2.4, 3.0,
      3.5, 3.6, 4.0, 4.7, 4.8, 5.0
    ))
    n2.weights should be (List(
      1.1, 1.2, 2.0, 2.3, 2.4, 3.0,
      3.5, 3.6, 4.0, 4.7, 4.8, 5.0
    ))
  }

  it should "construct the nn correctly" in {
    val n = NeuralNet.createRandom(
      numInputs = 4,
      numOutputs = 2,
      numHiddenLayers = 1,
      neuronsPerHiddenLayer = 6,
      neuronFactory=new NeuronFactory(10.0, 11.0)
    )
    n.layers.size should be (2) // input/hidden + output
    n.layers(0).neurons.size should be (6)
    n.layers(0).neurons(0).inputWeights.size should be (4)
    n.layers(1).neurons.size should be (2)
    n.layers(1).neurons(0).inputWeights.size should be (6)
  }
}
