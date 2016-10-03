package org.danielholmes.smartsweepers

import java.net.URISyntaxException
import java.nio.file.Paths
import java.time.Duration

import org.danielholmes.smartsweepers.ga.{GeneticAlgorithmEnvironment, Genome, SimulationFitness}
import org.danielholmes.smartsweepers.nn.{NeuralNetFactory, NeuronFactory}
import org.danielholmes.smartsweepers.original.CParams
import org.danielholmes.smartsweepers.sim.Size
import org.danielholmes.smartsweepers.ui.{ResultsGraphPanel, SimRunPanel}

import scala.swing._
import scala.swing.event.ButtonClicked

object Main extends SimpleSwingApplication {
  loadInConfigParameters()

  val simSize = Size(400, 400)
  var allResults: List[GenerationSummary] = List.empty
  var population: List[Genome] = List.empty
  var lastPopulation: List[Genome] = List.empty
  val nnFactory = new NeuralNetFactory(
    numOutputs = CParams.iNumOutputs,
    neuronsPerHiddenLayer = CParams.iNeuronsPerHiddenLayer,
    numHiddenLayers = CParams.iNumHidden,
    numInputs = CParams.iNumInputs,
    neuronFactory = new NeuronFactory(CParams.dBias, CParams.dActivationResponse)
  )
  val ga = new GeneticAlgorithmEnvironment(
    crossoverRate = 0.7,
    mutationRate = 0.1,
    numElites = 4,
    numEliteCopies = 1,
    maxPerturbation = 0.3,
    fitness = new SimulationFitness(
      size=simSize,
      numTicks=CParams.iNumTicks,
      numMines=CParams.iNumMines,
      neuralNetFactory=nnFactory
    )
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
  val gpsLabel = new Label

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
    contents += gpsLabel
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
      val runPanel = new SimRunPanel(
        simSize = simSize,
        numMines = CParams.iNumMines,
        framesPerSecond = CParams.iFramesPerSecond,
        brains = population.map(p => nnFactory.createFromWeights(p.weights))
      )
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
    population = List.fill(CParams.iNumSweepers) { Genome(nnFactory.createRandom().weights) }
    lastPopulation = population
  }

  private def runLoop(): Unit = {
    val start = System.currentTimeMillis()
    val newResults = ga.runGeneration(population)
    val timeTook = System.currentTimeMillis() - start
    results = results :+ GenerationSummary(newResults.maxFitness, newResults.averageFitness, Duration.ofMillis(timeTook))

    lastPopulation = newResults.performance.map(_.genome).toList
    population = newResults.nextPopulation

    draw()
  }

  def draw(): Unit = {
    startButton.visible = !_running
    stopButton.visible = _running
    resetButton.enabled = !_running

    def formatDouble(d: Double) = f"$d%1.1f"

    generationNumberLabel.text = "Gen: " + results.size
    maxFitnessLabel.text = "Max Fit: " + results.lastOption.map(_.maxFitness).map(formatDouble).getOrElse(0)
    averageFitnessLabel.text = "Ave Fit: " + results.lastOption.map(_.averageFitness).map(formatDouble).getOrElse(0)
    highestFitnessEverLabel.text = "All Time Max Fit: " + results.map(_.maxFitness).reduceOption(_ max _).map(formatDouble).getOrElse(0)
    if (results.nonEmpty) {
      val recentResults = results.takeRight(8)
      gpsLabel.text = "ms/Gen: " + recentResults.map(_.processingTime.toMillis).sum / recentResults.size
    } else {
      gpsLabel.text = ""
    }

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
