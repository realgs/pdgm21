import scala.concurrent.*
import scala.concurrent.duration.*
import ExecutionContext.Implicits.global
import Benchmark._

object Matrix {

  def main(args: Array[String]): Unit =

    //results
    //it depends on the size of matrices
    //for matrices smaller then 30x30 sequential computation is faster
    //for big matrices parallel computation is better even for 2-3 rows
    
    println("\nsmall matrices + low range (-100; 100)")
    println("---------------------------")
    var elementNr = 3
    while elementNr <= 30_000 do
      val array1 = generate2DArray(elementNr, 30, -100, 100)
      val array2 = generate2DArray(30, 40, -100, 100)

      println(elementNr + " rows test")
      println("sequential:\t" + measureTime(multiply(array1, array2)))
      println("parallel:\t" + measureTime(multiplyParallel(array1, array2)))
      elementNr *= 10

    println("\nbig matrices + low range (-100; 100)")
    println("---------------------------")
    elementNr = 3
    while elementNr <= 15 do
      val array1 = generate2DArray(elementNr, 10_000, -100, 100)
      val array2 = generate2DArray(10_000, 5000, -100, 100)

      println(elementNr + " rows test")
      println("sequential:\t" + measureTime(multiply(array1, array2)))
      println("parallel:\t" + measureTime(multiplyParallel(array1, array2)))
      elementNr *= 5

    println("\nsmall matrices + high range (-100_000; 100_000)")
    println("---------------------------")
    elementNr = 3
    while elementNr <= 30_000 do
      val array1 = generate2DArray(elementNr, 30, -100_000, 100_000)
      val array2 = generate2DArray(30, 40, -100_000, 100_000)

      println(elementNr + " rows test")
      println("sequential:\t" + measureTime(multiply(array1, array2)))
      println("parallel:\t" + measureTime(multiplyParallel(array1, array2)))
      elementNr *= 10

    println("\nbig matrices + high range (-100_000; 100_000)")
    println("---------------------------")
    elementNr = 2
    while elementNr <= 10 do
      val array1 = generate2DArray(elementNr, 10_000, -100_000, 100_000)
      val array2 = generate2DArray(10_000, 4000, -100_000, 100_000)

      println(elementNr + " rows test")
      println("sequential:\t" + measureTime(multiply(array1, array2)))
      println("parallel:\t" + measureTime(multiplyParallel(array1, array2)))
      elementNr *= 5


  def multiply(matrix1: Array[Array[Int]], matrix2: Array[Array[Int]]): Array[Array[Int]] =
    val matrix1Rows = matrix1.length
    val matrix1Col = matrix1(0).length
    val matrix2Rows = matrix2.length
    val matrix2Col = matrix2(0).length

    if(matrix1Col != matrix2Rows)
      throw new Exception("Invalid operation")

    val result = Array.ofDim[Int](matrix1Rows, matrix2Col)

    for(i <- 0 until matrix1Rows)
      for(j <- 0 until matrix2Col)
        result(i)(j) = 0
        for(m <- 0 until matrix1Col)
          result(i)(j) += matrix1(i)(m) * matrix2(m)(j)
    result

  def multiplyParallel(matrix1: Array[Array[Int]], matrix2: Array[Array[Int]]): Array[Array[Int]] =
    val matrix1Rows = matrix1.length
    val matrix1Col = matrix1(0).length
    val matrix2Rows = matrix2.length
    val matrix2Col = matrix2(0).length

    if(matrix1Col != matrix2Rows)
      throw new Exception("Invalid operation")

    val result = Array.ofDim[Int](matrix1Rows, matrix2Col)
    def multiplyHelper(row: Int): Unit =
      for (j <- 0 until matrix2Col)
        result(row)(j) = 0
        for (m <- 0 until matrix1Col)
          result(row)(j) += matrix1(row)(m) * matrix2(m)(j)

    val futures = for(i <- 0 until matrix1Rows) yield Future{
      multiplyHelper(i)
    }
    futures.foreach(Await.result(_, Duration.Inf))
    result
}
