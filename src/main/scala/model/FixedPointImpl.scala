package model

import breeze.linalg.{CSCMatrix, DenseMatrix, DenseVector, randn}
import breeze.numerics.{abs, sqrt}
import utils.MatrixUtils._

case class FixedPointImpl(maxIterations: Int = 200, verbose: Boolean = false) extends FixedPoint {
  def wiZANDual(R: CSCMatrix[Double], M: CSCMatrix[Double], N: CSCMatrix[Double], d: Int, w: Double = 0.1, p: Double = 0.01, lambda: Double = 0.1, lambdaUser: Double = 1.0, lambdaItem: Double = 1.0): (DenseMatrix[Double], DenseMatrix[Double]) = {
    val (m, n) = (R.rows, R.cols)

    val U = randn((m, d)) * 0.01
    val V = randn((n, d)) * 0.01

    U := abs(U)
    V := abs(V)

    val sparseR1 = R.copy

    def fillSparseR(): Unit = {
      val it = R.activeKeysIterator
      for ((i, j) <- it)
        sparseR1(i, j) = U(i, ::) * V(j, ::).t
    }

    val wp = w * p

    val negwpR = R * (1 - wp)
    val wpmCol = DenseVector.fill(m, wp)
    val onesnRow = DenseVector.ones[Double](n).t
    val lambdaUserM = M * lambdaUser
    val lambdaDm = degreeMatrix(M) * lambdaUser

    val negwpRt = R.t * (1 - wp)
    val wpnCol = DenseVector.fill(n, wp)
    val onesmRow = DenseVector.ones[Double](m).t
    val lambdaItemN = N * lambdaItem
    val lambdaDn = degreeMatrix(N) * lambdaItem

    for (iter <- 0 until maxIterations) {
      fillSparseR()

      val A1 = negwpR * V + wpmCol * (onesnRow * V) + lambdaUserM * U
      val B1 = (sparseR1 * (1 - w)) * V + w * U * (V.t * V) + lambda * U + lambdaDm * U
      U := U *:* sqrt(A1 /:/ B1)

      val A2 = negwpRt * U + wpnCol * (onesmRow * U) + lambdaItemN * V
      val B2 = (sparseR1.t * (1 - w)) * U + w * V * (U.t * U) + lambda * V + lambdaDn * V
      V := V *:* sqrt(A2 /:/ B2)
    }

    (U, V)
  }
}
