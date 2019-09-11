package evaluation

import java.io.{File, PrintWriter}

import breeze.linalg.{CSCMatrix, DenseMatrix}
import model.{WeightedALSImpl, FixedPointImpl}
import utils.{SparseReader, Validation}

object EfficiencyDensityEvaluation extends App {
  val OUTPUT_DIR = "metrics"
  val matrix = SparseReader.readAsSparse("movlen-sparse.csv")
  val userMatrix = CSCMatrix.zeros[Double](matrix.rows, matrix.rows)  // Movlens does not provide user side information
  val itemMatrix = SparseReader.readAsSparse("movlen-side-item-sparse.csv")

  type TrainFunction = (CSCMatrix[Double], CSCMatrix[Double]) => (DenseMatrix[Double], DenseMatrix[Double])

  def measureRuntime(evaluator: => (DenseMatrix[Double], DenseMatrix[Double])) = {
    val now = System.currentTimeMillis()
    val (u, v) = evaluator
    val time = (System.currentTimeMillis() - now) / 1000.0
    time
  }

  def measureModel(train: TrainFunction, filename: String): Unit = {
    val trainProportions = 50 to 100 by 10
    val pw = new PrintWriter(new File(s"$OUTPUT_DIR/$filename"))

    println(s"Evaluating $filename")
    println("proportion,size,time")
    pw.println("proportion,size,time")

    for (tp <- trainProportions) {
      val testRatio = (100 - tp) / 100.0
      val (trainSet, _) = Validation.trainTestSplit(matrix, testRatio = testRatio, seed = 42)
      val (trainUserSet, _) = Validation.trainTestSplit(userMatrix, testRatio = testRatio, seed = 42)

      val size = trainSet.activeSize
      val time = measureRuntime(train(trainSet, trainUserSet))
      pw.println(s"$tp,$size,$time")
      println(s"$tp,$size,$time")
    }

    pw.close()
  }

  val convexModel = WeightedALSImpl(maxIterations = 20)
  val fixedPointModel = FixedPointImpl(maxIterations = 180)

  val convexTrain: TrainFunction = (r, _) => convexModel.wiZANUniform(r.toDense, d = 20)
  val fixedPointTrain: TrainFunction = (r, u) => fixedPointModel.wiZANDual(r, u, itemMatrix, d = 20)

  measureModel(convexTrain, "convex-wizan-density.csv")
  measureModel(fixedPointTrain, "fixed-wizandual-density.csv")
}
