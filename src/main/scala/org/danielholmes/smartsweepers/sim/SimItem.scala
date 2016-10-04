package org.danielholmes.smartsweepers.sim

trait SimItem {
  def update(sim: Simulation): SimItem
}
