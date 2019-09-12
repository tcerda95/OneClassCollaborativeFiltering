package model

import breeze.linalg.{DenseMatrix, inv, randn, sum}
import breeze.numerics.pow
import utils.MatrixUtils._

import scala.collection.parallel.CollectionConverters._
import scala.language.postfixOps

case class WeightedALSImpl(maxIterations: Int = 15, verbose: Boolean = false) extends WeightedALS {
  override def ZAN(R: DenseMatrix[Double], d: Int, lambda: Double): (DenseMatrix[Double], DenseMatrix[Double]) = {
    def cost(U: DenseMatrix[Double], V: DenseMatrix[Double]): Double = {
      val diff = sum(pow(R - U * V.t, 2))

      val squaredNormU = sum(pow(U, 2))
      val squaredNormV = sum(pow(V, 2))
      val regCost = lambda * (squaredNormU + squaredNormV)

      diff + regCost
    }

    val (m, n) = (R.rows, R.cols)

    val U = randn((m, d)) * 0.01
    val V = randn((n, d)) * 0.01

    val lambdaI = sparseDiag(d, lambda)

    for (iter <- 0 until maxIterations) {
      if (verbose) println(s"Cost at iteration $iter: ${cost(U, V)}")

      val A1 = lambdaI + V.t * V
      for (i <- 0 until m par) {
        val b = R(i, ::) * V
        U(i, ::) := (A1.t \ b.t).t
      }

      val A2 = lambdaI + U.t * U
      for (j <- 0 until n par) {
        val b = R(::, j).t * U
        V(j, ::) := (A2.t \ b.t).t
      }
    }

    if (verbose) println(s"Cost at iteration $maxIterations: ${cost(U, V)}")

    (U, V)
  }

  override def wZAN(R: DenseMatrix[Double], W: DenseMatrix[Double], d: Int, lambda: Double = 0.01): (DenseMatrix[Double], DenseMatrix[Double]) = {
    def cost(U: DenseMatrix[Double], V: DenseMatrix[Double]) = {
      val diff = pow(R - U * V.t, 2)
      val wSum = sum(W *:* diff)

      val squaredNormU = sum(pow(U, 2))
      val squaredNormV = sum(pow(V, 2))
      val regCost = lambda * (squaredNormU + squaredNormV)

      wSum + regCost
    }

    val (m, n) = (R.rows, R.cols)

    val U = randn((m, d)) * 0.01
    val V = randn((n, d)) * 0.01

    val lambdaI = sparseDiag(d, lambda)

    val iwDiags = (0 until W.rows) map { i => sparseDiag(W(i, ::).t) }

    val jwDiags = (0 until W.cols) map { j => sparseDiag(W(::, j)) }

    for (iter <- 0 until maxIterations) {
      if (verbose) println(s"Cost at iteration $iter: ${cost(U, V)}")

      for (i <- 0 until m par) {
        val iwDiag = iwDiags(i)
        val wDiagsV = iwDiag * V
        val A = lambdaI + V.t * wDiagsV
        val b = R(i, ::) * wDiagsV
        U(i, ::) := (A.t \ b.t).t
      }

      for (j <- 0 until n par) {
        val jwDiag = jwDiags(j)
        val wDiagsU = jwDiag * U
        val A = lambdaI + U.t * wDiagsU
        val b = R(::, j).t * wDiagsU
        V(j, ::) := (A.t \ b.t).t
      }
    }

    if (verbose) println(s"Cost at iteration $maxIterations: ${cost(U, V)}")

    (U, V)
  }
}
