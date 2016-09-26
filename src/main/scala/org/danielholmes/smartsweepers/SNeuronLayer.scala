package org.danielholmes.smartsweepers

import java.util

class SNeuronLayer(var m_NumNeurons: Int, val NumInputsPerNeuron: Int) {
  var m_vecNeurons = new util.Vector[SNeuron]
  var i: Int = 0
  while (i < m_NumNeurons) {
    {
      m_vecNeurons.add(new SNeuron(NumInputsPerNeuron))
    }
    {
      i += 1; i
    }
  }
}
