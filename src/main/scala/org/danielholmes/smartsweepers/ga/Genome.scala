package org.danielholmes.smartsweepers.ga

import java.{lang, util}

class Genome() extends Comparable[Genome] {
  var vecWeights: util.List[Double] = new util.ArrayList[Double]
  var dFitness = 0.0

  def this(w: util.List[Double], f: Double) {
    this()
    vecWeights = w
    dFitness = f
  }

  def compareTo(other: Genome): Int = lang.Double.compare(dFitness, other.dFitness)
}
