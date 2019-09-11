package utils

import breeze.linalg.{CSCMatrix, DenseMatrix}

import scala.io.Source

object SparseReader {
  def readAsDense(filename: String): DenseMatrix[Double] = {
    val ((m, n), nonZeroPositions) = dimensionsAndNonZeroPositions(filename)

    val matrix = DenseMatrix.zeros[Double](m, n)
    matrix(nonZeroPositions.toSeq) := 1.0
    matrix
  }

  def readAsSparse(filename: String): CSCMatrix[Double] = {
    val ((m, n), nonZeroPositions) = dimensionsAndNonZeroPositions(filename)

    val builder = new CSCMatrix.Builder[Double](m, n)

    for ((i, j) <- nonZeroPositions)
      builder.add(i, j, 1.0)

    builder.result()
  }

  private def dimensionsAndNonZeroPositions(filename: String) = {
    val str = Source.fromFile(s"src/main/resources/$filename").toList.mkString
    val Array(dims, rows, cols) = str split "\n"
    val mn = toIntArray(dims)
    val (m, n) = (mn(0), mn(1))

    val nonZeroPositions = toIntArray(rows) zip toIntArray(cols)

    ((m, n), nonZeroPositions)
  }

  private def toIntArray(str: String): Array[Int] = (str split ",") map (_.toInt)

}
