package org.danielholmes.smartsweepers.sim

import org.danielholmes.smartsweepers.nn.{NeuralNet, Neuron, NeuronLayer}
import org.scalatest._

class MineSweeperSpec extends FlatSpec with Matchers {
  "MineSweeper" should "calculate exact position mine hitting correctly" in {
    val m = MineSweeper(
      brain = new NeuralNet(List(
        NeuronLayer(List(
          Neuron(List(0, 0, 0, 0), 0, 0, 1),
          Neuron(List(0, 0, 0, 0), 0, 0, 1)
        ))
      )),
      position = Vector2D(10, 10),
      rotation = 0
    )

    m.isHitting(Mine(Vector2D(10, 10))) should be (true)
  }

  it should "calculate at edge position mine hitting correctly" in {
    val m = MineSweeper(
      brain = new NeuralNet(List(
        NeuronLayer(List(
          Neuron(List(0, 0, 0, 0), 0, 0, 1),
          Neuron(List(0, 0, 0, 0), 0, 0, 1)
        ))
      )),
      position = Vector2D(10, 10),
      rotation = 0
    )

    m.isHitting(Mine(Vector2D(16.95, 10))) should be (true)
  }

  it should "calculate past edge position mine hitting correctly" in {
    val m = MineSweeper(
      brain = new NeuralNet(List(
        NeuronLayer(List(
          Neuron(List(0, 0, 0, 0), 0, 0, 1),
          Neuron(List(0, 0, 0, 0), 0, 0, 1)
        ))
      )),
      position = Vector2D(10, 10),
      rotation = 0
    )

    m.isHitting(Mine(Vector2D(17.05, 10))) should be (false)
  }
}
