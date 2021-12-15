package main.paradygmaty

import scala.concurrent.{Future, Await}
import scala.util.Random
import scala.annotation.tailrec
import scala.util.{Failure, Success}
import scala.concurrent.duration.Duration
import scala.concurrent.ExecutionContext.Implicits.global

object Matrix {

  private val random = Random

  def randomMatrixOfDim(numOfRows: Int, numOfColumns: Int, rangeMin: Double = 0, rangeMax: Double = 1): List[List[Double]] =
    if rangeMin>=rangeMax then throw Exception("Incorrect range exception!")
    List.fill(numOfRows)(List.fill(numOfColumns)(rangeMin + (rangeMax - rangeMin) * random.nextDouble() ))


  def print2DMatrix[A](matrix: List[List[A]]) =
    matrix.map(_.mkString(" ")).foreach(println)


  def findMaxInMatrix(matrix: List[List[Double]]): Double=
    var maxVar = Double.NegativeInfinity

    val max = matrix.foldLeft(maxVar){
      (max, x) =>
        x.foldLeft(max){
          (max, y) =>
            if y > max then
              maxVar = y
              maxVar
            else maxVar
        }
    }
    maxVar


  def findMaxInList(list: List[Double]): Double =
    var maxVar = Double.NegativeInfinity

    val max = list.foldLeft(maxVar){
      (max, x) =>
        if x > max then
          maxVar = x
          maxVar
        else maxVar

    }
    maxVar


  def findMaxInMatrixFuture(matrix: List[List[Double]]): Double=
    //var maxVar = Double.NegativeInfinity
    val matrixSize = matrix.size

    @tailrec
    def listOfFutures(listOfFut: List[Future[Double]], sizeLeft: Int): List[Future[Double]]=
      if sizeLeft <= 0 then listOfFut
      else
        listOfFutures(Future(findMaxInList(matrix(matrixSize-sizeLeft))) :: listOfFut, sizeLeft-1)

    val result = Future.sequence(listOfFutures(Nil, matrixSize))

    /*
    result onComplete {
      case Success(list) =>
        println(list)
        maxVar = findMaxInList(list)
      case Failure(t) => throw new Exception("Exception occured")
    }
    */

    findMaxInList(Await.result(result, Duration.Inf))

    //Await.ready(result, Duration.Inf)
    // maxVar


  def findMaxInMatrixQuadPowerFuture(matrix: List[List[Double]]): Double=
    val stepSize = (matrix.size / 5) + 1


    val f1 = Future { findMaxInMatrix( matrix.slice(0, stepSize) )}
    val f2 = Future { findMaxInMatrix( matrix.slice(stepSize, 2*stepSize) )}
    val f3 = Future { findMaxInMatrix( matrix.slice(2*stepSize, 3*stepSize) )}
    val f4 = Future { findMaxInMatrix( matrix.slice(3*stepSize, 4*stepSize+1) )}

    val result = for {
      r1 <- f1
      r2 <- f2
      r3 <- f3
      r4 <- f4
    } yield (r1 :: r2 :: r3 :: r4 :: Nil)

    findMaxInList(Await.result(result, Duration.Inf))


}

