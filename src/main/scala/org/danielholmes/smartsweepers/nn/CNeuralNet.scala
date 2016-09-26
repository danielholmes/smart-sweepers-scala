package org.danielholmes.smartsweepers.nn

import java.util

import org.danielholmes.smartsweepers.CParams
import scala.collection.JavaConverters._

class CNeuralNet() {
  private val m_NumInputs = CParams.iNumInputs
  private val m_NumOutputs = CParams.iNumOutputs
  private val numHiddenLayers = CParams.iNumHidden
  private val m_NeuronsPerHiddenLyr = CParams.iNeuronsPerHiddenLayer
  private val layers = new util.ArrayList[NeuronLayer]

  CreateNet()

  /**
    * this method builds the ANN. The weights are all initially set to
    * random values -1 < w < 1
    */
  private def CreateNet() {
    //create the layers of the network
    if (numHiddenLayers > 0) {
      //create first hidden layer
      layers.add(NeuronLayer(m_NeuronsPerHiddenLyr, m_NumInputs))
      var i: Int = 0
      while (i < numHiddenLayers - 1) {
        {
          layers.add(NeuronLayer(m_NeuronsPerHiddenLyr, m_NeuronsPerHiddenLyr))
        }
        {
          i += 1; i
        }
      }
      layers.add(NeuronLayer(m_NumOutputs, m_NeuronsPerHiddenLyr))
    }
    else layers.add(NeuronLayer(m_NumOutputs, m_NumInputs))
  }

  /**
    * returns a vector containing the weights
    */
  def GetWeights: util.List[Double] = {
    layers.asScala
      .flatMap(_.neurons)
      .flatMap(_.inputWeights)
      .asJava
  }

  def putWeights(weights: util.List[Double]) {
    var cWeight: Int = 0
    for (i <- 0 to numHiddenLayers) {
      val newNeurons = new util.ArrayList[Neuron]
      for (j <- 0 until layers.get(i).numNeurons) {
        val newWeights = new util.ArrayList[Double]
        for (k <- 0 until layers.get(i).neurons(j).numInputs) {
          newWeights.add(weights.get(cWeight))
          cWeight += 1
        }
        newNeurons.add(Neuron(newWeights.asScala.toList))
      }

      layers.set(i, NeuronLayer(newNeurons.asScala.toList))
    }
  }

  def numberOfWeights: Int = layers.asScala.flatMap(_.neurons).map(_.numInputs).sum

  def update(firstInputs: List[Double]): List[Double] = {
    //first check that we have the correct amount of inputs
    require(firstInputs.size == m_NumInputs)

    var inputs: List[Double] = firstInputs
    //stores the resultant outputs from each layer
    var outputs: List[Double] = Nil

    for (i <- 0 to numHiddenLayers) {
      if (i > 0) inputs = outputs
      outputs = layers.get(i).getOutputs(inputs)
    }
    outputs
  }
}
