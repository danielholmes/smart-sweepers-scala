package org.danielholmes.smartsweepers

import java.util

class SNeuron(val NumInputs: Int) {
  var m_vecWeight = new util.Vector[Double]
  var m_NumInputs = NumInputs + 1
  //we need an additional weight for the bias hence the +1
  var i: Int = 0
  while (i < NumInputs + 1) {
    {
      //set up the weights with an initial random value
      m_vecWeight.add(Utils.RandomClamped)
    }
    {
      i += 1; i
    }
  }
}
