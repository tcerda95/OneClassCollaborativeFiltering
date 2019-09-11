package utils

import breeze.linalg.{CSCMatrix, DenseMatrix, DenseVector, Vector, argsort, argtopk, min, sum}
import breeze.numerics.pow
import breeze.stats.mean

import scala.collection.parallel.CollectionConverters._

object Metrics {
  def meanAveragePrecisionK(R: CSCMatrix[Double], U: DenseMatrix[Double], V: DenseMatrix[Double], k: Int): Double = {
    require((R.rows, R.cols) == (U.rows, V.rows), s"Dimesion mismatch ${(R.rows, R.cols)} != ${(U.rows, V.cols)}")

    val colRange = 0 until R.cols

    val averagePrecisions = (0 until R.rows).par map { i => averagePrecisionK(R(i, colRange).t.toDenseVector, (U(i, ::) * V.t).t) }
    averagePrecisions.sum / averagePrecisions.size
  }

  def meanAveragePrecisionK(R: CSCMatrix[Double], X: DenseMatrix[Double], k: Int): Double = {
    meanAveragePrecisionK(R.toDense, X, k)
  }

  def meanAveragePrecisionK(R: DenseMatrix[Double], X: DenseMatrix[Double], k: Int): Double = {
    require((R.rows, R.cols) == (X.rows, X.cols), s"Dimesion mismatch ${(R.rows, R.cols)} != ${(X.rows, X.cols)}")

    val averagePrecisions = (0 until R.rows).par map { i => averagePrecisionK(R(i, ::).t, X(i, ::).t, k) }
    averagePrecisions.sum / averagePrecisions.size
  }

  def averagePrecisionK(r: Vector[Double], x: Vector[Double], k: Int = 10): Double = {
    val kk = min(x.size, k)
    val topIdx = argtopk(x, kk)
    val rTop = r(topIdx)
    val relevant = sum(rTop)

    if (relevant == 0) 0.0
    else {
      val sumRelevantPrecisions: Double = sum((0 until kk) map { i => if (rTop(i) == 1.0) precisionK(rTop, i) else 0.0 })
      sumRelevantPrecisions / relevant
    }
  }

  def precisionK(x: Vector[Double], k: Int): Double = {
    val vector = x.toDenseVector
    mean(vector(0 to k))
  }

  def halfLifeUtility(R: CSCMatrix[Double], U: DenseMatrix[Double], V: DenseMatrix[Double], halflife: Int): Double = {
    require((R.rows, R.cols) == (U.rows, V.rows), s"Dimesion mismatch ${(R.rows, R.cols)} != ${(U.rows, V.cols)}")

    val colRange = 0 until R.cols

    val usersHlus = (0 until R.rows).par map { i => userHlu(R(i, colRange).t.toDenseVector, (U(i, ::) * V.t).t, halflife) }

    val maxHlus = (0 until R.rows).par map { i => maxHlu(R(i, colRange).t.toDenseVector, halflife) }

    100 * (usersHlus.sum / maxHlus.sum)
  }

  def halfLifeUtility(R: CSCMatrix[Double], X: DenseMatrix[Double], halflife: Int): Double = {
    halfLifeUtility(R.toDense, X, halflife)
  }

  def halfLifeUtility(R: DenseMatrix[Double], X: DenseMatrix[Double], halflife: Int = 5): Double = {
    require((R.rows, R.cols) == (X.rows, X.cols), s"Dimesion mismatch ${(R.rows, R.cols)} != ${(X.rows, X.cols)}")

    val usersHlus = (0 until R.rows) map { i => userHlu(R(i, ::).t, X(i, ::).t, halflife) }

    val maxHlus = (0 until R.rows) map { i => maxHlu(R(i, ::).t, halflife) }

    100 * (sum(usersHlus) / sum(maxHlus))
  }

  private def userHlu(r: Vector[Double], x: Vector[Double], halflife: Int): Double = {
    val topIdx = argsort(x).reverse
    val rTop = r(topIdx)

    hluSum(rTop, halflife)
  }

  private def maxHlu(r: DenseVector[Double], halflife: Int): Double = {
    val onlyPreferred = r(r :== 1.0)

    hluSum(onlyPreferred, halflife)
  }

  private def hluSum(v: Vector[Double], halflife: Int): Double = {
    sum((0 until v.size) map { i => v(i) / pow(2, i / (halflife - 1.0)) })
  }
}
