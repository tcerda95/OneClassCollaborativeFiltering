package utils

import breeze.linalg.{*, CSCMatrix, DenseMatrix, DenseVector, sum}

object MatrixUtils {
  def sparseDiag(vector: DenseVector[Double]): CSCMatrix[Double] = {
    val n = vector.length
    val builder = new CSCMatrix.Builder[Double](n, n)

    for (i <- 0 until n)
      builder.add(i, i, vector(i))

    builder.result()
  }

  def sparseDiag(n: Int, scalar: Double): CSCMatrix[Double] = {
    val builder = new CSCMatrix.Builder[Double](n, n)

    for (i <- 0 until n)
      builder.add(i, i, scalar)

    builder.result()
  }

  def degreeMatrix(M: DenseMatrix[Double]): CSCMatrix[Double] = {
    val (m, n) = (M.rows, M.cols)

    require(m == n, s"Matrix must be square. Dimesions mismatch: $m != $n")

    val degrees = sum(M(*, ::))

    sparseDiag(degrees)
  }

  def degreeMatrix(M: CSCMatrix[Double]): CSCMatrix[Double] = {
    val (m, n) = (M.rows, M.cols)
    require(m == n, s"Matrix must be square. Dimesions mismatch: $m != $n")

    val degrees = DenseVector.zeros[Double](m)

    for (((i, _), v) <- M.activeIterator)
      degrees(i) += v

    sparseDiag(degrees)
  }
}
