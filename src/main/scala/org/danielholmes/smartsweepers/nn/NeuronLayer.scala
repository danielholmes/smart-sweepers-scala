package org.danielholmes.smartsweepers.nn

import java.util
import scala.collection.JavaConverters._

class NeuronLayer(var numNeurons: Int, val numInputsPerNeuron: Int) {
  var neurons = new util.ArrayList[Neuron]
  for (i <- 0 until numNeurons) {
    neurons.add(Neuron(numInputsPerNeuron))
  }

  def getOutputs(inputs: List[Double]): List[Double] = {
    neurons.asScala.map(_.getActivation(inputs)).toList
  }
}
