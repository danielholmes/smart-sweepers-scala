package org.danielholmes.smartsweepers

import java.util
import java.lang

class SGenome() extends Comparable[SGenome] {
  var vecWeights = new util.Vector[Double]
  var dFitness = 0.0

  def this(w: util.Vector[Double], f: Double) {
    this()
    vecWeights = w
    dFitness = f
  }

  def compareTo(other: SGenome): Int = lang.Double.compare(dFitness, other.dFitness)
}
