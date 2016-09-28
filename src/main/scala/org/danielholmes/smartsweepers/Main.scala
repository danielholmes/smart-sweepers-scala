package org.danielholmes.smartsweepers

import java.net.URISyntaxException
import java.nio.file.Paths

import org.danielholmes.smartsweepers.ga.{GeneticAlgorithmEnvironment, Genome, LegacyFitness}
import org.danielholmes.smartsweepers.nn.{NeuralNet, NeuronFactory}
import org.danielholmes.smartsweepers.original.{CParams, Utils}
import org.danielholmes.smartsweepers.sim.{MineSweeper, Size, Vector2D}
import org.danielholmes.smartsweepers.ui.{ResultsGraphPanel, SimRunPanel}

import scala.swing._
import scala.swing.event.ButtonClicked

object Main extends SimpleSwingApplication {
  loadInConfigParameters()

  var allResults: List[GenerationSummary] = Nil
  val ga = new GeneticAlgorithmEnvironment(
    crossoverRate = 0.7,
    mutationRate = 0.1,
    numElites = 4,
    numEliteCopies = 1,
    maxPerturbation = 0.3,
    fitness = new LegacyFitness()
  )

  var _running = false
  var results: List[GenerationSummary] = Nil

  object resetButton extends Button { text = "Reset" }
  object startButton extends Button { text = "Start" }
  object stopButton extends Button { text = "Stop" }
  object openSimButton extends Button { text = "See Current Generation" }
  listenTo(resetButton)
  listenTo(startButton)
  listenTo(stopButton)
  listenTo(openSimButton)

  val generationNumberLabel = new Label
  val maxFitnessLabel = new Label
  val averageFitnessLabel = new Label
  val highestFitnessEverLabel = new Label

  val toolBar = new FlowPanel {
    maximumSize = new Dimension(500, 30)

    contents += resetButton
    contents += startButton
    contents += stopButton
    contents += openSimButton
  }
  val statsBar = new FlowPanel {
    maximumSize = new Dimension(500, 30)

    contents += generationNumberLabel
    contents += maxFitnessLabel
    contents += averageFitnessLabel
    contents += highestFitnessEverLabel
  }

  val graphPanel = new ResultsGraphPanel {
    minimumSize = new Dimension(500, 400)
  }

  def top = new MainFrame() {
    title = "Smart Sweepers"
    minimumSize = new Dimension(500, 500)
    contents = new BoxPanel(Orientation.Vertical) {
      contents += toolBar
      contents += statsBar
      contents += graphPanel
    }
  }

  reactions += {
    case ButtonClicked(`resetButton`) => reset()
    case ButtonClicked(`startButton`) => setRunning(true)
    case ButtonClicked(`stopButton`) => setRunning(false)
    case ButtonClicked(`openSimButton`) => openSim()
  }

  def setRunning(running: Boolean): Unit = {
    _running = running
    draw()
  }

  // Old style TODO: Refactor
  val simSize = Size(400, 400)
  private var m_vecThePopulation: List[Genome] = List.empty
  private var m_vecSweepers: List[MineSweeper] = List.empty
  private var m_vecMines: List[Vector2D] = List.empty
  // End old style

  val runner = new Thread(new Runnable {
    def run() = {
      while (true) {
        print("") // Don't know why, removing this stops it working! loop just kind of disappears, like something is gcd
        if (_running) {
          runLoop()
        }
      }
    }
  })

  override def main(args: Array[String]): Unit = {
    super.main(args)

    reset()
    runner.start()
  }

  private def reset(): Unit = {
    results = List.empty
    createSimulation()
    draw()
  }

  private def openSim(): Unit = {
    val simFrame = new Frame {
      title = s"Smart Sweepers - Generation ${results.size}"
      minimumSize = new Dimension(400, 400)
      val runPanel = new SimRunPanel(m_vecSweepers.map(_.brain))
      runPanel.minimumSize = minimumSize
      contents = runPanel

      override def closeOperation(): Unit = {
        super.closeOperation()
        runPanel.dispose()
      }
    }
    simFrame.open()
  }

  private def createSimulation(): Unit = {
    val brains = List.fill(CParams.iNumSweepers) {
      NeuralNet.createRandom(
        numOutputs = CParams.iNumOutputs,
        neuronsPerHiddenLayer = CParams.iNeuronsPerHiddenLayer,
        numHiddenLayers = CParams.iNumHidden,
        numInputs = CParams.iNumInputs,
        neuronFactory = new NeuronFactory(CParams.dBias, CParams.dActivationResponse)
      )
    }
    m_vecThePopulation = brains.map(b => Genome(b.weights, 0))
    m_vecSweepers = brains.map(new MineSweeper(_))
    m_vecMines = List.fill(CParams.iNumMines) { Vector2D(Utils.RandFloat * simSize.width, Utils.RandFloat * simSize.height) }
  }

  private def runLoop(): Unit = {
    for (tick <- 0 until CParams.iNumTicks) {
      m_vecThePopulation = m_vecSweepers.indices
        .map(i => {
          val g = m_vecThePopulation(i)
          val s = m_vecSweepers(i)
          s.update(m_vecMines)

          val GrabHit: Int = s.checkForMine(m_vecMines, CParams.dMineScale)
          if (GrabHit >= 0) {
            s.incrementFitness()
            m_vecMines = m_vecMines.slice(0, GrabHit) ++
              List(Vector2D(Utils.RandFloat * simSize.width, Utils.RandFloat * simSize.height)) ++
              m_vecMines.slice(GrabHit + 1, m_vecMines.size)
          }

          Genome(g.weights, s.fitness)
        })
        .toList
    }

    val newResults = ga.runGeneration(m_vecThePopulation)
    results = results :+ GenerationSummary(newResults.maxFitness, newResults.averageFitness)
    m_vecThePopulation = newResults.nextPopulation
    for (i <- m_vecSweepers.indices) {
      val s = m_vecSweepers(i)
      s.putWeights(m_vecThePopulation(i).weights)
      s.reset()
    }

    draw()
  }

  def draw(): Unit = {
    startButton.visible = !_running
    stopButton.visible = _running
    resetButton.enabled = !_running

    def formatDouble(d: Double) = f"$d%1.1f"

    generationNumberLabel.text = "Gen: " + results.size
    maxFitnessLabel.text = "Max Fitness: " + results.lastOption.map(_.maxFitness).map(formatDouble).getOrElse(0)
    averageFitnessLabel.text = "Ave Fitness: " + results.lastOption.map(_.averageFitness).map(formatDouble).getOrElse(0)
    highestFitnessEverLabel.text = "Highest Ever Fitness: " + results.map(_.maxFitness).reduceOption(_ max _).map(formatDouble).getOrElse(0)

    graphPanel.results = results
  }

  private def loadInConfigParameters() {
    try {
      CParams.LoadInParameters(Paths.get(Thread.currentThread.getContextClassLoader.getResource("params.ini").toURI))
    } catch {
      case e: URISyntaxException => throw new RuntimeException(e)
    }
  }
}
