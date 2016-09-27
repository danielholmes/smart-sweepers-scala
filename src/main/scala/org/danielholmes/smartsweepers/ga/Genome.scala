package org.danielholmes.smartsweepers.ga

import java.{lang, util}

class Genome(w: List[Double], f: Double) extends Comparable[Genome] {
  var weights: List[Double] = w
  var dFitness = f

  def compareTo(other: Genome): Int = lang.Double.compare(dFitness, other.dFitness)
}
