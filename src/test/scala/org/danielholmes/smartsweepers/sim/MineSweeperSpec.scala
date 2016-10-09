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

  it should "collectMine while keeping data" in {
    val brain = new NeuralNet(List(
      NeuronLayer(List(
        Neuron(List(0, 0, 0, 0), 0, 0, 1),
        Neuron(List(0, 0, 0, 0), 0, 0, 1)
      ))
    ))
    val m = MineSweeper(
      brain = brain,
      position = Vector2D(10, 10),
      rotation = 45
    )
    val changed = m.collectMine()
    changed.position should be (Vector2D(10, 10))
    changed.brain should be (brain)
    changed.rotation should be (45)
    changed.numMinesCollected should be (1)
  }
}
