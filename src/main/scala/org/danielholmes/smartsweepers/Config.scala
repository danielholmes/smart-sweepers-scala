package org.danielholmes.smartsweepers

import java.io.IOException
import java.lang.Double
import java.net.URISyntaxException
import java.nio.file.{Files, Path, Paths}

import scala.annotation.tailrec
import scala.collection.JavaConverters._

case class Config(
  areaWidth: Int,
  areaHeight: Int,
  framesPerSecond: Int,
  numHiddenLayers: Int,
  numNeuronsPerHiddenLayer: Int,
  numOutputs: Int,
  activationResponse: Double,
  bias: Double,
  numSweepers: Int,
  numMines: Int,
  numRocks: Int,
  numTicks: Int,
  crossoverRate: Double,
  // Try figures around 0.05 to 0.3 ish
  mutationRate: Double,
  maxPerturbation: Double,
  numElite: Int,
  numCopiesElite: Int
) {
  require((numCopiesElite * numElite % 2) == 0)
}

object Config {
  def loadFromResource(name: String): Config = {
    try {
      loadFromPath(Paths.get(Thread.currentThread.getContextClassLoader.getResource(name).toURI))
    } catch {
      case e: URISyntaxException => throw new RuntimeException(e)
    }
  }

  def loadFromPath(path: Path): Config = {
    try {
      loadFromLines(Files.lines(path).iterator().asScala.toList)
    }
    catch {
      case e: IOException => throw new RuntimeException("Error getting ini file", e)
    }
  }

  def loadFromLines(lines: List[String]): Config = {
    @tailrec
    def linesToMap(current: Map[String, String], remaining: List[String]): Map[String, String] = {
      remaining match {
        case Nil => current
        case x :: xs => {
          val parts = x.split(" ")
          if (parts.length != 2) throw new RuntimeException(s"Invalid line: $x")

          linesToMap(current.updated(parts(0), parts(1)), xs)
        }
      }
    }

    loadFromMap(linesToMap(Map.empty, lines.filter(line => line != null && !line.equals(""))))
  }

  def loadFromMap(raw: Map[String, String]): Config = {
    Config(
      areaWidth = Integer.parseInt(raw.getOrElse("WindowWidth", "400")),
      areaHeight = Integer.parseInt(raw.getOrElse("WindowHeight", "400")),
      framesPerSecond = Integer.parseInt(raw("iFramesPerSecond")),
      numHiddenLayers = Integer.parseInt(raw("iNumHidden")),
      numNeuronsPerHiddenLayer = Integer.parseInt(raw("iNeuronsPerHiddenLayer")),
      numOutputs = Integer.parseInt(raw("iNumOutputs")),
      activationResponse = Double.parseDouble(raw("dActivationResponse")),
      bias = Double.parseDouble(raw("dBias")),
      numSweepers = Integer.parseInt(raw("iNumSweepers")),
      numMines = Integer.parseInt(raw("iNumMines")),
      numRocks = Integer.parseInt(raw("iNumRocks")),
      numTicks = Integer.parseInt(raw("iNumTicks")),
      crossoverRate = Double.parseDouble(raw("dCrossoverRate")),
      mutationRate = Double.parseDouble(raw("dMutationRate")),
      maxPerturbation = Double.parseDouble(raw("dMaxPerturbation")),
      numElite = Integer.parseInt(raw("iNumElite")),
      numCopiesElite = Integer.parseInt(raw("iNumCopiesElite"))
    )
  }
}
