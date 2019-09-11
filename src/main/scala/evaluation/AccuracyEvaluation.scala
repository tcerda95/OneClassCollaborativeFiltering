package evaluation

import java.io.{File, PrintWriter}

import breeze.linalg.{CSCMatrix, DenseMatrix}
import utils.Metrics._
import model.{WeightedALSImpl, FixedPointImpl}
import utils.{SparseReader, Validation}


object AccuracyEvaluation extends App {
  val OUTPUT_DIR = "metrics"
  val matrix = SparseReader.readAsSparse("movlen-sparse.csv")
  val userMatrix = CSCMatrix.zeros[Double](matrix.rows, matrix.rows)  // Movlens does not provide user side information
  val itemMatrix = SparseReader.readAsSparse("movlen-side-item-sparse.csv")

  val (trainSet, testSet) = Validation.trainTestSplit(matrix, testRatio = 0.2, seed = 42)
  val denseTrainSet = trainSet.toDense

  val convexModel = WeightedALSImpl(maxIterations = 20)
  val fixedPointModel = FixedPointImpl(maxIterations = 180)

  type TrainFunction = (Int, Double, Double) => (DenseMatrix[Double], DenseMatrix[Double])

  def measureUV(u: DenseMatrix[Double], v: DenseMatrix[Double]) = {
    val X = u * v.t

    (meanAveragePrecisionK(trainSet, X, 10),
      meanAveragePrecisionK(testSet, X, 10),
      halfLifeUtility(trainSet, X, 5),
      halfLifeUtility(testSet, X, 5))
  }

  def measureModel(train: TrainFunction, filename: String): Unit = {
    val ds = 15 to 20
    val ws = List(0.1, 0.2)
    val ps = List(0.01, 0.1)

    val pw = new PrintWriter(new File(s"$OUTPUT_DIR/$filename"))

    println(s"Evaluating $filename")
    println("d,w,p,mapTrain,mapTest,hluTrain,hluTest")
    pw.println("d,w,p,mapTrain,mapTest,hluTrain,hluTest")

    for (d <- ds; w <- ws; p <- ps) {
      val (u, v) = train(d, w, p)
      val (mapTrain, mapTest, hluTrain, hluTest) = measureUV(u, v)

      println(s"$d,$w,$p,$mapTrain,$mapTest,$hluTrain,$hluTest")
      pw.println(s"$d,$w,$p,$mapTrain,$mapTest,$hluTrain,$hluTest")
    }

    pw.close()
  }

  val convexTrain: TrainFunction = (d, w, p) => convexModel.wiZANUniform(denseTrainSet, d, w, p)
  val fixedPointTrain: TrainFunction = (d, w, p) => fixedPointModel.wiZANDual(trainSet, userMatrix, itemMatrix, d, w, p)

  measureModel(convexTrain, "convex-wizan.csv")
  measureModel(fixedPointTrain, "fixed-wizandual.csv")
}
