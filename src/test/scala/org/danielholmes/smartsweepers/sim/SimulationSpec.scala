package org.danielholmes.smartsweepers.sim

import org.danielholmes.smartsweepers.nn.{NeuralNet, Neuron, NeuronLayer}
import org.scalatest._

class SimulationSpec extends FlatSpec with Matchers {
  "Simulation" should "process hit mine correctly" in {
    val sim = Simulation(
      Size(400, 400),
      List(
        MineSweeper(
          brain = new NeuralNet(List(
            NeuronLayer(List(
              Neuron(List(0, 0, 0, 0, 0, 0), 0, 0, 1),
              Neuron(List(0, 0, 0, 0, 0, 0), 0, 0, 1)
            ))
          )),
          position = Vector2D(10, 10),
          rotation = 0
        ),
        Rock(Vector2D(10, 10)),
        Mine(Vector2D(10, 10))
      )
    )

    val newSim = sim.update()
    newSim.mines.size should be (1)
    newSim.sweepers.size should be (1)
    newSim.sweepers.head.numMinesCollected should be (1)
  }

  it should "process hit rock correctly" in {
    val sim = Simulation(
      Size(400, 400),
      List(
        MineSweeper(
          brain = new NeuralNet(List(
            NeuronLayer(List(
              Neuron(List(0, 0, 0, 0, 0, 0), 0, 0, 1),
              Neuron(List(0, 0, 0, 0, 0, 0), 0, 0, 1)
            ))
          )),
          position = Vector2D(10, 10),
          rotation = 0
        ),
        Rock(Vector2D(10, 10)),
        Mine(Vector2D(100, 100)) // Mine is mandatory atm
      )
    )

    val newSim = sim.update()
    newSim.rocks.size should be (1)
    newSim.sweepers.size should be (1)
    newSim.sweepers.head.numRocksHit should be (1)
  }
}
