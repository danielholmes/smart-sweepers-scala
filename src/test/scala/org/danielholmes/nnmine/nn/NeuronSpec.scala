package org.danielholmes.nnmine.nn

import org.danielholmes.smartsweepers.nn.Neuron
import org.scalatest._

class NeuronSpec extends FlatSpec with Matchers {
  "Neuron" should "return correct activation for 0" in {
    Neuron(List(0, 0), 0, -1, 1).getActivation(List(1, 1)) should be (0.5)
  }

  it should "return correct activation for > 0" in {
    val value = Neuron(List(1, 1), 1, -1, 1).getActivation(List(1, 1))
    value should be > 0.5
    value should be < 1.0
  }

  it should "return correct activation for < 0" in {
    val value = Neuron(List(0, 0), 1, -1, 1).getActivation(List(0, 0))
    value should be > 0.0
    value should be < 0.5
  }
}
