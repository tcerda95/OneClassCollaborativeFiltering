package utils

import breeze.linalg.{DenseMatrix, argtopk}

object RecommendationUtils {
  def recommend(U: DenseMatrix[Double], V: DenseMatrix[Double], userRow: Int, limit: Int = 50): IndexedSeq[Int] = {
    val r = U(userRow, ::) * V.t
    val topIdx = argtopk(r.t, limit)
    topIdx
  }
}
