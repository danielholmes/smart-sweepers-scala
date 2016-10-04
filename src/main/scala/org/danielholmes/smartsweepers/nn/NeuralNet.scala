package org.danielholmes.smartsweepers.nn

import scala.annotation.tailrec

class NeuralNet(val layers: List[NeuronLayer]) {
  require(layers.nonEmpty)

  private val numInputs = layers.head.numInputs

  lazy val weights: List[Double] = layers.flatMap(_.neurons).flatMap(_.allWeights)

  def totalNumberOfWeights: Int = layers.flatMap(_.neurons).map(_.numberOfWeights).sum

  def update(inputs: List[Double]): List[Double] = {
    require(inputs.size == numInputs)

    @tailrec
    def update(remainingLayers: List[NeuronLayer], currentInputs: List[Double]): List[Double] = {
      remainingLayers match {
        case Nil => currentInputs
        case x :: xs => update(xs, x.getOutputs(currentInputs))
      }
    }

    update(layers, inputs)
  }
}
