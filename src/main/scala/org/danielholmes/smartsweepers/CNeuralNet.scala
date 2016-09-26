package org.danielholmes.smartsweepers

import java.util

class CNeuralNet() {
  private var m_NumInputs = CParams.iNumInputs
  private var m_NumOutputs = CParams.iNumOutputs
  private var m_NumHiddenLayers = CParams.iNumHidden
  private var m_NeuronsPerHiddenLyr = CParams.iNeuronsPerHiddenLayer
  private var m_vecLayers = new util.Vector[SNeuronLayer]
  CreateNet()

  /**
    * this method builds the ANN. The weights are all initially set to
    * random values -1 < w < 1
    */
  private def CreateNet() {
    //create the layers of the network
    if (m_NumHiddenLayers > 0) {
      //create first hidden layer
      m_vecLayers.add(new SNeuronLayer(m_NeuronsPerHiddenLyr, m_NumInputs))
      var i: Int = 0
      while (i < m_NumHiddenLayers - 1) {
        {
          m_vecLayers.add(new SNeuronLayer(m_NeuronsPerHiddenLyr, m_NeuronsPerHiddenLyr))
        }
        {
          i += 1; i
        }
      }
      m_vecLayers.add(new SNeuronLayer(m_NumOutputs, m_NeuronsPerHiddenLyr))
    }
    else m_vecLayers.add(new SNeuronLayer(m_NumOutputs, m_NumInputs))
  }

  /**
    * returns a vector containing the weights
    */
  def GetWeights: util.Vector[Double] = {
    //this will hold the weights
    val weights: util.Vector[Double] = new util.Vector[Double]
    //for each layer
    var i: Int = 0
    while (i < m_NumHiddenLayers + 1) {
      {
        //for each neuron
        var j: Int = 0
        while (j < m_vecLayers.get(i).m_NumNeurons) {
          {
            //for each weight
            var k: Int = 0
            while (k < m_vecLayers.get(i).m_vecNeurons.get(j).m_NumInputs) {
              {
                weights.add(m_vecLayers.get(i).m_vecNeurons.get(j).m_vecWeight.get(k))
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
    * given a vector of doubles this function replaces the weights in the NN
    * with the new values
    */
  def PutWeights(weights: util.Vector[Double]) {
    var cWeight: Int = 0
    //for each layer
    var i: Int = 0
    while (i < m_NumHiddenLayers + 1) {
      {
        //for each neuron
        var j: Int = 0
        while (j < m_vecLayers.get(i).m_NumNeurons) {
          {
            //for each weight
            var k: Int = 0
            while (k < m_vecLayers.get(i).m_vecNeurons.get(j).m_NumInputs) {
              {
                m_vecLayers.get(i).m_vecNeurons.get(j).m_vecWeight.set(k, weights.get({
                  cWeight += 1; cWeight - 1
                }))
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
  }

  /**
    * returns the total number of weights needed for the net
    */
  def GetNumberOfWeights: Int = {
    var weights: Int = 0
    //for each layer
    var i: Int = 0
    while (i < m_NumHiddenLayers + 1) {
      {
        //for each neuron
        var j: Int = 0
        while (j < m_vecLayers.get(i).m_NumNeurons) {
          {
            //for each weight
            var k: Int = 0
            while (k < m_vecLayers.get(i).m_vecNeurons.get(j).m_NumInputs) {
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
  def Update(firstInputs: util.Vector[Double]): util.Vector[Double] = {
    //first check that we have the correct amount of inputs
    require(firstInputs.size == m_NumInputs)

    var inputs: util.Vector[Double] = firstInputs
    //stores the resultant outputs from each layer
    val outputs: util.Vector[Double] = new util.Vector[Double]
    var cWeight: Int = 0
    //For each layer....
    var i: Int = 0
    while (i < m_NumHiddenLayers + 1) {
      {
        if (i > 0) inputs = outputs.clone.asInstanceOf[util.Vector[Double]]
        outputs.clear()
        cWeight = 0
        //for each neuron sum the (inputs * corresponding weights).Throw
        //the total at our sigmoid function to get the output.
        var j: Int = 0
        while (j < m_vecLayers.get(i).m_NumNeurons) {
          {
            var netinput: Double = 0
            val NumInputs: Int = m_vecLayers.get(i).m_vecNeurons.get(j).m_NumInputs
            //for each weight
            var k: Int = 0
            while (k < NumInputs - 1) {
              {
                //sum the weights x inputs
                netinput += m_vecLayers.get(i).m_vecNeurons.get(j).m_vecWeight.get(k) * inputs.get({
                  cWeight += 1; cWeight - 1
                })
              }
              {
                k += 1; k
              }
            }
            //add in the bias
            netinput += m_vecLayers.get(i).m_vecNeurons.get(j).m_vecWeight.get(NumInputs - 1) * CParams.dBias
            //we can store the outputs from each layer as we generate them.
            //The combined activation is first filtered through the sigmoid
            //function
            outputs.add(Sigmoid(netinput, CParams.dActivationResponse))
            cWeight = 0
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
    outputs
  }

  private def Sigmoid(netinput: Double, response: Double): Double = 1 / (1 + Math.exp(-netinput / response))
}
