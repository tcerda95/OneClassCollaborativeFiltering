package evaluation

import java.io.{File, PrintWriter}

import breeze.linalg.{CSCMatrix, DenseMatrix}
import breeze.numerics.round
import model.{WeightedALSImpl, FixedPointImpl}
import utils.{SparseReader}

object EfficiencyMatrixSizeEvaluation extends App {
  val OUTPUT_DIR = "metrics"
  val matrix = SparseReader.readAsSparse("movlen-sparse.csv")
  val userMatrix = CSCMatrix.zeros[Double](matrix.rows, matrix.rows)  // Movlens does not provide user side information
  val itemMatrix = SparseReader.readAsSparse("movlen-side-item-sparse.csv")

  type TrainFunction = (CSCMatrix[Double], CSCMatrix[Double], CSCMatrix[Double]) => (DenseMatrix[Double], DenseMatrix[Double])

  def measureRuntime(evaluator: => (DenseMatrix[Double], DenseMatrix[Double])) = {
    val now = System.currentTimeMillis()
    val (u, v) = evaluator
    val time = (System.currentTimeMillis() - now) / 1000.0
    time
  }

  def slice(matrix: CSCMatrix[Double], proportion: Double): CSCMatrix[Double] = {
    val rows = round(matrix.rows * proportion).toInt
    val cols = round(matrix.cols * proportion).toInt
    val positions = matrix(0 until rows, 0 until cols).activeIterator

    val builder = new CSCMatrix.Builder[Double](rows, cols)

    for (((i, j), value) <- positions) {
      if (value > 0.0) {
        builder.add(i, j, 1.0)
      }
    }

    builder.result()
  }

  def measureModel(train: TrainFunction, filename: String): Unit = {
    val trainProportions = 50 to 100 by 10
    val pw = new PrintWriter(new File(s"$OUTPUT_DIR/$filename"))

    println(s"Evaluating $filename")
    println("proportion,m,n,time")
    pw.println("proportion,m,n,time")

    for (tp <- trainProportions) {
      val sliced = slice(matrix, tp / 100.0)
      val slicedUser = slice(userMatrix, tp / 100.0)
      val slicedItem = slice(itemMatrix, tp / 100.0)

      val m = sliced.rows
      val n = sliced.cols
      val time = measureRuntime(train(sliced, slicedUser, slicedItem))
      pw.println(s"$tp,$m,$n,$time")
      println(s"$tp,$m,$n,$time")
    }

    pw.close()
  }

  val convexModel = WeightedALSImpl(maxIterations = 20)
  val fixedPointModel = FixedPointImpl(maxIterations = 180)

  val convexTrain: TrainFunction = (r, _, _) => convexModel.wiZANUniform(r.toDense, d = 20)
  val fixedPointTrain: TrainFunction = (r, u, i) => fixedPointModel.wiZANDual(r, u, i, d = 20)

  measureModel(convexTrain, "convex-wizan-msize.csv")
  measureModel(fixedPointTrain, "fixed-wizandual-msize.csv")
}
