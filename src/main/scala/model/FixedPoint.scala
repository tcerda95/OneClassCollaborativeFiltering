package model

import breeze.linalg.{CSCMatrix, DenseMatrix}

trait FixedPoint {
  def wiZANDual(R: CSCMatrix[Double], M: CSCMatrix[Double], N: CSCMatrix[Double], d: Int, w: Double, p: Double, lambda: Double = 0.01, lambdaUser: Double = 1.0, lambdaItem: Double = 0.1): (DenseMatrix[Double], DenseMatrix[Double])

  def iZANDual(R: CSCMatrix[Double], M: CSCMatrix[Double], N: CSCMatrix[Double], d: Int, p: Double, lambda: Double = 0.01, lambdaUser: Double = 1.0, lambdaItem: Double = 0.1): (DenseMatrix[Double], DenseMatrix[Double]) = {
    wiZANDual(R, M, N, d, 1.0, p, lambda, lambdaUser, lambdaItem)
  }

  def wiZAN(R: CSCMatrix[Double], d: Int, w: Double, p: Double, lambda: Double = 0.01): (DenseMatrix[Double], DenseMatrix[Double]) = {
    val M = CSCMatrix.zeros[Double](R.rows, R.rows)
    val N = CSCMatrix.zeros[Double](R.cols, R.cols)
    wiZANDual(R, M, N, d, w, p, lambda, 0, 0)
  }

  def iZAN(R: CSCMatrix[Double], d: Int, p: Double, lambda: Double = 0.01): (DenseMatrix[Double], DenseMatrix[Double]) = {
    val M = CSCMatrix.zeros[Double](R.rows, R.rows)
    val N = CSCMatrix.zeros[Double](R.cols, R.cols)
    wiZANDual(R, M, N, d, 1.0, p, lambda, 0, 0)
  }

  private def fillZeros(A: DenseMatrix[Double], value: Double) = {
    val B = A.copy
    B(B :== 0.0) := value
    B
  }
}
