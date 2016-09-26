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
      layers.add(new NeuronLayer(m_NeuronsPerHiddenLyr, m_NumInputs))
      var i: Int = 0
      while (i < numHiddenLayers - 1) {
        {
          layers.add(new NeuronLayer(m_NeuronsPerHiddenLyr, m_NeuronsPerHiddenLyr))
        }
        {
          i += 1; i
        }
      }
      layers.add(new NeuronLayer(m_NumOutputs, m_NeuronsPerHiddenLyr))
    }
    else layers.add(new NeuronLayer(m_NumOutputs, m_NumInputs))
  }

  /**
    * returns a vector containing the weights
    */
  def GetWeights: util.List[Double] = {
    //this will hold the weights
    val weights: util.List[Double] = new util.ArrayList[Double]
    //for each layer
    var i: Int = 0
    while (i < numHiddenLayers + 1) {
      {
        //for each neuron
        var j: Int = 0
        while (j < layers.get(i).numNeurons) {
          {
            //for each weight
            var k: Int = 0
            while (k < layers.get(i).neurons.get(j).numInputs) {
              {
                weights.add(layers.get(i).neurons.get(j).inputWeights(k))
              }
              {
                k += 1; k
              }
            }
          }
          {
            j += 1; j
          }
        }
      }
      {
        i += 1; i
      }
    }
    weights
  }

  def putWeights(weights: util.List[Double]) {
    var cWeight: Int = 0
    for (i <- 0 to numHiddenLayers) {
      for (j <- 0 until layers.get(i).numNeurons) {
        val newWeights = new util.ArrayList[Double]
        for (k <- 0 until layers.get(i).neurons.get(j).numInputs) {
          newWeights.add(weights.get(cWeight))
          cWeight += 1
        }
        layers.get(i).neurons.set(j, Neuron(newWeights.asScala.toList))
      }
    }
  }

  def numberOfWeights: Int = {
    var weights: Int = 0
    //for each layer
    var i: Int = 0
    while (i < numHiddenLayers + 1) {
      {
        //for each neuron
        var j: Int = 0
        while (j < layers.get(i).numNeurons) {
          {
            //for each weight
            var k: Int = 0
            while (k < layers.get(i).neurons.get(j).numInputs) {
              {
                weights += 1; weights - 1
              }
              {
                k += 1; k
              }
            }
          }
          {
            j += 1; j
          }
        }
      }
      {
        i += 1; i
      }
    }
    weights
  }

  /**
    * given an input vector this function calculates the output vector
    */
  def Update(firstInputs: util.List[Double]): util.List[Double] = {
    //first check that we have the correct amount of inputs
    require(firstInputs.size == m_NumInputs)

    var inputs: List[Double] = firstInputs.asScala.toList
    //stores the resultant outputs from each layer
    var outputs: List[Double] = Nil

    for (i <- 0 to numHiddenLayers) {
      if (i > 0) inputs = outputs
      outputs = layers.get(i).getOutputs(inputs)
    }
    outputs.asJava
  }
}
