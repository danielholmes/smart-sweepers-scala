package org.danielholmes.smartsweepers

class Point() {
  var x: Double = 0.0
  var y: Double = 0.0

  def this(a: Float, b: Float) {
    this()
    x = a
    y = b
  }
}
