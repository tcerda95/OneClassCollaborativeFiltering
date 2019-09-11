package model

import breeze.linalg.DenseMatrix

trait WeightedALS {
  def wZAN(R: DenseMatrix[Double], W: DenseMatrix[Double], d: Int, lambda: Double = 0.01): (DenseMatrix[Double], DenseMatrix[Double])

  def ZAM(R: DenseMatrix[Double], d: Int, lambda: Double = 0.01): (DenseMatrix[Double], DenseMatrix[Double]) = {
    val W = R
    wZAN(R, W, d, lambda)
  }

  def ZAN(R: DenseMatrix[Double], d: Int, lambda: Double = 0.01): (DenseMatrix[Double], DenseMatrix[Double]) = {
    val W = fillZeros(R, 1.0)
    wZAN(R, W, d, lambda)
  }

  def iZANUniform(R: DenseMatrix[Double], d: Int, p: Double = 0.01, lambda: Double): (DenseMatrix[Double], DenseMatrix[Double]) = {
    val P = fillZeros(R, p)
    ZAN(P, d, lambda)
  }

  def iZAN(R: DenseMatrix[Double], P: DenseMatrix[Double], d: Int, lambda: Double): (DenseMatrix[Double], DenseMatrix[Double]) = {
    ZAN(R + P, d, lambda)
  }

  def wiZANUniform(R: DenseMatrix[Double], d: Int, w: Double = 0.1, p: Double = 0.01, lambda: Double = 0.01): (DenseMatrix[Double], DenseMatrix[Double]) = {
    val W = fillZeros(R, w)
    val P = fillZeros(R, p)
    wZAN(P, W, d, lambda)
  }

  def wiZAN(R: DenseMatrix[Double], W: DenseMatrix[Double], P: DenseMatrix[Double], d: Int, lambda: Double = 0.01): (DenseMatrix[Double], DenseMatrix[Double]) =
    wZAN(R + P, W, d, lambda)

  def wZANUniform(R: DenseMatrix[Double], d: Int, w: Double = 0.1, lambda: Double = 0.01): (DenseMatrix[Double], DenseMatrix[Double]) = {
    val W = fillZeros(R, w)
    wZAN(R, W, d, lambda)
  }

  def fillZeros(A: DenseMatrix[Double], value: Double): DenseMatrix[Double] = {
    val B = A.copy
    B(B :== 0.0) := value
    B
  }
}
