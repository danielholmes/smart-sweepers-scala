package org.danielholmes.smartsweepers.nn

import java.util

class SNeuronLayer(var numNeurons: Int, val NumInputsPerNeuron: Int) {
  var neurons = new util.Vector[Neuron]
  var i: Int = 0
  while (i < numNeurons) {
    {
      neurons.add(new Neuron(NumInputsPerNeuron))
    }
    {
      i += 1; i
    }
  }
}
