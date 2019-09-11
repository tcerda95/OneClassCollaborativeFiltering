package utils

import breeze.linalg.{CSCMatrix, DenseMatrix}

import scala.util.Random
import scala.language.postfixOps


object Validation {
  def trainTestSplit(R: DenseMatrix[Double], testSize: Double, seed: Int): (DenseMatrix[Double], DenseMatrix[Double]) =
    trainTestSplit(R, testSize, new Random(seed))

  def trainTestSplit(R: DenseMatrix[Double], testSize: Double): (DenseMatrix[Double], DenseMatrix[Double]) =
    trainTestSplit(R, testSize, new Random)

  private def trainTestSplit(R: DenseMatrix[Double], testSize: Double, random: Random): (DenseMatrix[Double], DenseMatrix[Double]) = {
    val (n, m) = (R.rows, R.cols)
    val nonZeros = nonZerosPositions(R)
    val shuffled = random.shuffle(nonZeros)

    val testAmount = (nonZeros.size * testSize).toInt
    val (testPositions, trainPositions) = shuffled splitAt testAmount

    val test = DenseMatrix.zeros[Double](n, m)
    test(testPositions) := 1.0

    (onesAt(trainPositions, n, m), onesAt(testPositions, n, m))
  }

  private def nonZerosPositions(X: DenseMatrix[Double]) = {
    val nonZerosKV = (X :!= 0.0).iterator filter { case (k, isNonZero) => isNonZero }
    nonZerosKV map { case (position, _) => position } toVector
  }

  private def onesAt(positions: Seq[(Int, Int)], rows: Int, cols: Int) = {
    val A = DenseMatrix.zeros[Double](rows, cols)
    A(positions) := 1.0
    A
  }

  def trainTestSplit(R: CSCMatrix[Double], testRatio: Double, seed: Int): (CSCMatrix[Double], CSCMatrix[Double]) =
    trainTestSplit(R, testRatio, new Random(seed))

  def trainTestSplit(R: CSCMatrix[Double], testRatio: Double): (CSCMatrix[Double], CSCMatrix[Double]) =
    trainTestSplit(R, testRatio, new Random)

  def trainTestSplit(R: CSCMatrix[Double], testRatio: Double, random: Random): (CSCMatrix[Double], CSCMatrix[Double]) = {
    val (n, m) = (R.rows, R.cols)
    val nonZeros = nonZerosPositions(R)
    val shuffled = random.shuffle(nonZeros)

    val testAmount = (nonZeros.size * testRatio).toInt
    val (testPositions, trainPositions) = shuffled splitAt testAmount

    (onesAtSparse(trainPositions, n, m), onesAtSparse(testPositions, n, m))
  }

  private def nonZerosPositions(R: CSCMatrix[Double]) = {
    R.activeKeysIterator.toVector
  }

  private def onesAtSparse(positions: Seq[(Int, Int)], rows: Int, cols: Int) = {
    val builder = new CSCMatrix.Builder[Double](rows, cols)

    for ((i, j) <- positions)
      builder.add(i, j, 1.0)

    builder.result()
  }
}
