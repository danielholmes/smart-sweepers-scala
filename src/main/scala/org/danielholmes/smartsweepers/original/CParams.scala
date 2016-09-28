package org.danielholmes.smartsweepers.original

import java.io.IOException
import java.lang.Double
import java.nio.file.{Files, Path}

import scala.collection.JavaConverters._

object CParams {
  def LoadInParameters(path: Path) {
    try {
      for (line <- Files.lines(path).iterator().asScala) {
        if (line != null && !line.equals("")) {
          val parts = line.split(" ")
          if (parts.length != 2) throw new RuntimeException("Invalid line: " + line)

          val key = parts(0)
          val value = parts(1)
          key match {
            case "iFramesPerSecond" => iFramesPerSecond = Integer.parseInt(value)
            case "iNumInputs" => iNumInputs = Integer.parseInt(value)
            case "iNumHidden" => iNumHidden = Integer.parseInt(value)
            case "iNeuronsPerHiddenLayer" => iNeuronsPerHiddenLayer = Integer.parseInt(value)
            case "iNumOutputs" => iNumOutputs = Integer.parseInt(value)
            case "dActivationResponse" => dActivationResponse = Double.parseDouble(value)
            case "dBias" => dBias = Double.parseDouble(value)
            case "dMaxTurnRate" => dMaxTurnRate = Double.parseDouble(value)
            case "dMaxSpeed" => dMaxSpeed = Double.parseDouble(value)
            case "iSweeperScale" => iSweeperScale = Integer.parseInt(value)
            case "iNumMines" => iNumMines = Integer.parseInt(value)
            case "iNumSweepers" => iNumSweepers = Integer.parseInt(value)
            case "iNumTicks" => iNumTicks = Integer.parseInt(value)
            case "dMineScale" => dMineScale = Double.parseDouble(value)
            case "dCrossoverRate" => dCrossoverRate = Double.parseDouble(value)
            case "dMutationRate" => dMutationRate = Double.parseDouble(value)
            case "dMaxPerturbation" => dMaxPerturbation = Double.parseDouble(value)
            case "iNumElite" => iNumElite = Integer.parseInt(value)
            case "iNumCopiesElite" => iNumCopiesElite = Integer.parseInt(value)
            case _ => throw new RuntimeException("Unexpected key " + key)
          }
        }
      }
      require((CParams.iNumCopiesElite * CParams.iNumElite % 2) == 0)
    }
    catch {
      case e: IOException => throw new RuntimeException("Error getting ini file", e)
    }
  }

  var dPi: Double = 3.14159265358979
  var dHalfPi: Double = dPi / 2
  var dTwoPi: Double = dPi * 2
  var WindowWidth: Int = 400
  var WindowHeight: Int = 400
  var iFramesPerSecond: Int = 0
  var iNumInputs: Int = 0
  var iNumHidden: Int = 0
  var iNeuronsPerHiddenLayer: Int = 0
  var iNumOutputs: Int = 0
  var dActivationResponse: Double = 1.0
  var dBias: Double = -1.0
  var dMaxTurnRate: Double = 0.0
  var dMaxSpeed: Double = 0.0
  var iSweeperScale: Int = 0
  var iNumSweepers: Int = 0
  var iNumMines: Int = 0
  var iNumTicks: Int = 0
  var dMineScale: Double = 0.0
  var dCrossoverRate: Double = 0.0
  //Try figures around 0.05 to 0.3 ish
  var dMutationRate: Double = 0.0
  var dMaxPerturbation: Double = 0.0
  var iNumElite: Int = 0
  var iNumCopiesElite: Int = 0
}
