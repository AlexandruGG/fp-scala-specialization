package calculator

import Math.{sqrt, pow}

object Polynomial extends PolynomialInterface {
  def computeDelta(a: Signal[Double], b: Signal[Double], c: Signal[Double]): Signal[Double] = {
    Signal(pow(b(), 2) - 4 * a() * c())
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double], c: Signal[Double],
                       delta: Signal[Double]): Signal[Set[Double]] = {
    Signal {
      val d = delta()
      if (d < 0) Set()
      else {
        val aValue = a()
        val bValue = b()
        Set((-bValue + sqrt(d)) / (2 * aValue), (-bValue - sqrt(d)) / (2 * aValue))
      }
    }
  }
}
