package org.danielholmes.smartsweepers

import java.time.Duration

import org.danielholmes.smartsweepers.ga._
import org.danielholmes.smartsweepers.nn.{NeuralNetFactory, NeuronFactory}
import org.danielholmes.smartsweepers.sim.Size
import org.danielholmes.smartsweepers.ui.{ResultsGraphPanel, SimRunPanel}

import scala.swing._
import scala.swing.event.ButtonClicked

object Main extends SimpleSwingApplication {
  val config = Config.loadFromResource("params.ini")

  val simSize = Size(config.areaWidth, config.areaHeight)
  var allResults: List[GenerationSummary] = List.empty
  var population: List[Genome] = List.empty
  var lastResults: Option[GenerationResults] = None
  val nnFactory = new NeuralNetFactory(
    numOutputs = config.numOutputs,
    neuronsPerHiddenLayer = config.numNeuronsPerHiddenLayer,
    numHiddenLayers = config.numHiddenLayers,
    numInputs = 4,
    neuronFactory = new NeuronFactory(config.bias, config.activationResponse)
  )
  // Evolves spinning behaviour
  /*val fitness = new AvoidMinesFitness(
    size=simSize,
    numTicks=config.numTicks,
    numMines=config.numMines,
    neuralNetFactory=nnFactory
  )*/
  val fitness = new CollectMinesFitness(
    size=simSize,
    numTicks=config.numTicks,
    numMines=config.numMines,
    neuralNetFactory=nnFactory
  )
  /*val fitness = new AvoidMinesWhileTravellingFitness(
    size=simSize,
    numTicks=config.numTicks,
    numMines=config.numMines,
    neuralNetFactory=nnFactory
  )*/
  // Scrapped for the moment - requires more inputs (closest rock)
  /*val fitness = new CollectMinesAvoidRocksFitness(
    size=simSize,
    numTicks=config.numTicks,
    numMines=config.numMines,
    numRocks=config.numRocks,
    neuralNetFactory=nnFactory
  )*/
  val ga = new GeneticAlgorithmEnvironment(
    crossoverRate = config.crossoverRate,
    mutationRate = config.mutationRate,
    numElites = config.numElite,
    numEliteCopies = config.numCopiesElite,
    maxPerturbation = config.maxPerturbation,
    fitness = fitness
  )

  var _running = false
  var results: List[GenerationSummary] = Nil

  object resetButton extends Button { text = "Reset" }
  object startButton extends Button { text = "Start" }
  object stopButton extends Button { text = "Stop" }
  object openSimButton extends Button { text = "See Last Generation" }
  listenTo(startButton)
  listenTo(stopButton)
  listenTo(resetButton)
  listenTo(openSimButton)

  val generationNumberLabel = new Label
  val maxFitnessLabel = new Label
  val averageFitnessLabel = new Label
  val highestFitnessEverLabel = new Label
  val gpsLabel = new Label

  val frameDimensions = new Dimension(600, 600)
  val toolBar = new FlowPanel {
    maximumSize = new Dimension(frameDimensions.width, 30)

    contents += startButton
    contents += stopButton
    contents += resetButton
    contents += openSimButton
  }
  val statsBar = new FlowPanel {
    maximumSize = new Dimension(frameDimensions.width, 30)

    contents += generationNumberLabel
    contents += maxFitnessLabel
    contents += averageFitnessLabel
    contents += highestFitnessEverLabel
    contents += gpsLabel
  }

  val graphPanel = new ResultsGraphPanel {
    minimumSize = new Dimension(frameDimensions.width, frameDimensions.height - 100)
  }

  def top = new MainFrame() {
    title = "Smart Sweepers"
    minimumSize = frameDimensions
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
      minimumSize = new Dimension(simSize.width, simSize.height)
      val runPanel = new SimRunPanel(
        simSize = simSize,
        numMines = config.numMines,
        numRocks = config.numRocks,
        framesPerSecond = config.framesPerSecond,
        results = lastResults.get.performance,
        nnFactory = nnFactory
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
    population = List.fill(config.numSweepers) { Genome(nnFactory.createRandom().weights) }
    lastResults = None
  }

  private def runLoop(): Unit = {
    val start = System.currentTimeMillis()
    val newResults = ga.runGeneration(population)
    val timeTook = System.currentTimeMillis() - start
    results = results :+ GenerationSummary(newResults.maxFitness, newResults.averageFitness, Duration.ofMillis(timeTook))

    lastResults = Some(newResults)
    population = newResults.nextPopulation

    draw()
  }

  def draw(): Unit = {
    startButton.visible = !_running
    stopButton.visible = _running
    resetButton.enabled = !_running
    openSimButton.enabled = lastResults.isDefined

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
}
