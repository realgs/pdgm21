import scala.concurrent.Await
import scala.concurrent.Future
import scala.concurrent.duration.Duration
import scala.concurrent.ExecutionContext.Implicits.global
import scala.Array

object Lab7 {
  // Helper functions
  def createMatrix(noRows: Int, noColumns: Int): Array[Array[Int]] = {
    var matrix = Array.ofDim[Int](noRows, noColumns)

    var index = 1
    for (i <- 0 to noRows-1)
      for ( j <- 0 to noColumns-1) {
        matrix(i)(j) = index
        index += 1
      }

    return matrix
  }

  def createArray(length: Int): Array[Int] = {
    var outputArray = Array.ofDim[Int](length)

    var index = 1
    for (i <- 0 to length-1) {
      outputArray(i) = index
      index += 1
    }

    return outputArray
  }

  // Problem 1 - multiplying matrices
  // No paraller
  def multiplyMatrices(matrix1: Array[Array[Int]], matrix2: Array[Array[Int]]): Array[Array[Int]] = {
    var outputMatrix = Array.ofDim[Int](matrix1.size, matrix2(0).asInstanceOf[Array[Int]].size)

    for (i <- 0 to outputMatrix.length-1)
      for ( j <- 0 to outputMatrix(0).asInstanceOf[Array[Int]].length-1)
        outputMatrix(i)(j) = calculateDotProduct(matrix1, matrix2, i, j)

    return outputMatrix
  }

  def calculateDotProduct(array1: Array[Array[Int]], array2: Array[Array[Int]], rowArray1: Int,
                          columnArray2: Int): Int = {
    var result = 0
    for (i <- 0 to array2.length-1)
      result += array1(rowArray1)(i) * array2(i)(columnArray2)
    return result
  }

  // Paraller (we will manipulate on 6 equally separated parts of the array at the same time)
  def multiplyMatricesParaller(matrix1: Array[Array[Int]], matrix2: Array[Array[Int]]): Array[Array[Int]] = {
    def multiplyPartOfMatrices(outputMatrix: Array[Array[Int]], matrix1: Array[Array[Int]], matrix2: Array[Array[Int]],
                               startRow: Int, endRow: Int): Unit = {
      for (i <- startRow to endRow-1)
        for (j <- 0 to outputMatrix(0).asInstanceOf[Array[Int]].length-1)
          outputMatrix(i)(j) = calculateDotProduct(matrix1, matrix2, i, j)
    }

    var outputMatrix = Array.ofDim[Int](matrix1.length, matrix2(0).asInstanceOf[Array[Int]].length)

    // We can do this because we pass the reference to the array
    val f1 = Future{multiplyPartOfMatrices(outputMatrix, matrix1, matrix2,
      0, outputMatrix.length / 6)}
    val f2 = Future{multiplyPartOfMatrices(outputMatrix, matrix1, matrix2,
      outputMatrix.length / 6, outputMatrix.length / 3)}
    val f3 = Future{multiplyPartOfMatrices(outputMatrix, matrix1, matrix2,
      outputMatrix.length / 3, outputMatrix.length / 2)}
    val f4 = Future{multiplyPartOfMatrices(outputMatrix, matrix1, matrix2,
      outputMatrix.length / 2, outputMatrix.length * 2 / 3)}
    val f5 = Future{multiplyPartOfMatrices(outputMatrix, matrix1, matrix2,
      outputMatrix.length * 2 / 3, outputMatrix.length * 5 / 6)}
    val f6 = Future{multiplyPartOfMatrices(outputMatrix, matrix1, matrix2,
      outputMatrix.length * 5 / 6, outputMatrix.length)}

    return outputMatrix
  }

  // Problem 2 - Calculate determinant
  // No paraller
  // Note that the matrix needs to be of shape n x n
  def calculateDeterminant(matrix: Array[Array[Int]]): Int = {
    if matrix.length == 1 then matrix(0)(0)
    else {
      var result = 0
      var multiplier = 1
      for (i <- 0 to matrix.length - 1) {
        result += matrix(0)(i) * calculateDeterminant(getPartMatrix(matrix, 0, i)) * multiplier
        multiplier *= -1
      }
      return result
    }
  }

  def getPartMatrix(matrix: Array[Array[Int]], skipRowIndex: Int, skipColumnIndex: Int): Array[Array[Int]] = {
    var outputMatrix = Array.ofDim[Int](matrix.length-1, matrix.length-1)
    // Equal to 1 if the row or column needs to be skipped
    var rowSkip = 0
    var columnSkip = 0

    for (i <- 0 to outputMatrix.length-1) {
      if i == skipRowIndex then rowSkip = 1
      for ( j <- 0 to outputMatrix.length-1) {
        if j == skipColumnIndex then columnSkip = 1
        outputMatrix(i)(j) = matrix(i+rowSkip)(j+columnSkip)
      }

      // Reset the column skip
      columnSkip = 0
    }

    return outputMatrix
  }

  // Paraller
  def calculateDeterminantParaller(matrix: Array[Array[Int]]): Int = {
    def calculateDeterminantInner(matrix: Array[Array[Int]], startRow: Int, endRow: Int): Int = {
      var result = 0
      var multiplier = if startRow % 2 == 0 then 1 else -1
      for (i <- startRow to endRow - 1) {
        // We don't want to create many paraller processes
        result += matrix(0)(i) * calculateDeterminant(getPartMatrix(matrix, 0, i)) * multiplier
        multiplier *= -1
      }
      return result
    }

    if matrix.size == 1 then matrix(0)(0)
    else {
      val f1 = Future{calculateDeterminantInner(matrix, 0, matrix.length / 6)}
      val f2 = Future{calculateDeterminantInner(matrix, matrix.length / 6, matrix.length / 3)}
      val f3 = Future{calculateDeterminantInner(matrix, matrix.length / 3, matrix.length / 2)}
      val f4 = Future{calculateDeterminantInner(matrix, matrix.length / 2, matrix.length * 2 / 3)}
      val f5 = Future{calculateDeterminantInner(matrix, matrix.length * 2 / 3, matrix.length * 5 / 6)}
      val f6 = Future{calculateDeterminantInner(matrix, matrix.length * 5 / 6, matrix.length)}

      return Await.result(f1, Duration.Inf) + Await.result(f2, Duration.Inf) + Await.result(f3, Duration.Inf) +
        Await.result(f4, Duration.Inf) + Await.result(f5, Duration.Inf) + Await.result(f6, Duration.Inf)
    }
  }

  // Task 3 - summing arrays
  // No paraller
  def sumArray(arr: Array[Int]): Int = {
    var sum = 0
    for (i <- 0 to arr.length-1) {
      sum += arr(i)
    }

    return sum
  }

  // Paraller
  def sumArrayParaller(arr: Array[Int]): Int = {
    def sumArrayInner(arr: Array[Int], indexStart: Int, indexEnd: Int): Int = {
      var sum = 0
      for (i <- indexStart to indexEnd-1) {
        sum += arr(i)
      }

      return sum
    }

    val f1 = Future{sumArrayInner(arr, 0, arr.length / 6)}
    val f2 = Future{sumArrayInner(arr, arr.length / 6, arr.length / 3)}
    val f3 = Future{sumArrayInner(arr, arr.length / 3, arr.length / 2)}
    val f4 = Future{sumArrayInner(arr, arr.length / 2, arr.length * 2 / 3)}
    val f5 = Future{sumArrayInner(arr, arr.length * 2 / 3, arr.length * 5 / 6)}
    val f6 = Future{sumArrayInner(arr, arr.length * 5 / 6, arr.length)}

    return Await.result(f1, Duration.Inf) + Await.result(f2, Duration.Inf) + Await.result(f3, Duration.Inf) +
      Await.result(f4, Duration.Inf) + Await.result(f5, Duration.Inf) + Await.result(f6, Duration.Inf)
  }

  // I choose to divide everything into 6 parts because I have 6 cores
  def main(args: Array[String]): Unit = {
    val testMatrix1 = createMatrix(4, 6)
    val testMatrix2 = createMatrix(6, 4)
    val outputMatrix = multiplyMatrices(testMatrix1, testMatrix2)
    val outputMatrix2 = multiplyMatricesParaller(testMatrix1, testMatrix2)
    println("Correct output matrix")
    for (i <- 0 to 3) {
      for ( j <- 0 to 3) {
        print(" " + outputMatrix(i)(j));
      }
      println();
    }
    println()
    for (i <- 0 to 3) {
      for ( j <- 0 to 3) {
        print(" " + outputMatrix2(i)(j));
      }
      println();
    }

    println("\nCorrect determinant")
    println(calculateDeterminant(createMatrix(4, 4)) == 0)
    println(calculateDeterminant(Array(Array(1, 0, 4, -6), Array(2, 5, 0, 3),
      Array(-1, 2, 3, 5), Array(2, 1, -2, 3))) == 318)
    println(calculateDeterminantParaller(createMatrix(4, 4)) == 0)
    println(calculateDeterminantParaller(Array(Array(1, 0, 4, -6), Array(2, 5, 0, 3),
      Array(-1, 2, 3, 5), Array(2, 1, -2, 3))) == 318)

    println("\nCorrect sum")
    println(sumArray(createArray(10)) == 55)
    println(sumArrayParaller(createArray(10)) == 55)


    println("\nTests on task 1 - multiplying matrices")
    println("Matrices 10 x 10")
    var matrix1 = createMatrix(10, 10)
    var matrix2 = createMatrix(10, 10)
    var time = System.nanoTime()
    multiplyMatrices(matrix1, matrix2)
    println("Time without paraller: " + (System.nanoTime() - time).toString)
    time = System.nanoTime()
    multiplyMatricesParaller(matrix1, matrix2)
    println("Time with paraller:    " + (System.nanoTime() - time).toString)

    println("\nMatrices 100 x 100")
    matrix1 = createMatrix(100, 100)
    matrix2 = createMatrix(100, 100)
    time = System.nanoTime()
    multiplyMatrices(matrix1, matrix2)
    println("Time without paraller: " + (System.nanoTime() - time).toString)
    time = System.nanoTime()
    multiplyMatricesParaller(matrix1, matrix2)
    println("Time with paraller:    " + (System.nanoTime() - time).toString)

    println("\nMatrices 1000 x 1000")
    matrix1 = createMatrix(1000, 1000)
    matrix2 = createMatrix(1000, 1000)
    time = System.nanoTime()
    multiplyMatrices(matrix1, matrix2)
    println("Time without paraller: " + (System.nanoTime() - time).toString)
    time = System.nanoTime()
    multiplyMatricesParaller(matrix1, matrix2)
    println("Time with paraller:    " + (System.nanoTime() - time).toString)

    println("\nMatrices 2000 x 2000")
    matrix1 = createMatrix(2000, 2000)
    matrix2 = createMatrix(2000, 2000)
    time = System.nanoTime()
    multiplyMatrices(matrix1, matrix2)
    println("Time without paraller: " + (System.nanoTime() - time).toString)
    time = System.nanoTime()
    multiplyMatricesParaller(matrix1, matrix2)
    println("Time with paraller:    " + (System.nanoTime() - time).toString)

    println("\nTests on task 2 - calculate determinant")
    println("Matrices 3 x 3")
    var matrix = createMatrix(3, 3)
    time = System.nanoTime()
    calculateDeterminant(matrix)
    println("Time without paraller: " + (System.nanoTime() - time).toString)
    time = System.nanoTime()
    calculateDeterminantParaller(matrix)
    println("Time with paraller:    " + (System.nanoTime() - time).toString)

    println("\nMatrices 6 x 6")
    matrix = createMatrix(6, 6)
    time = System.nanoTime()
    calculateDeterminant(matrix)
    println("Time without paraller: " + (System.nanoTime() - time).toString)
    time = System.nanoTime()
    calculateDeterminantParaller(matrix)
    println("Time with paraller:    " + (System.nanoTime() - time).toString)

    println("\nMatrices 10 x 10")
    matrix = createMatrix(10, 10)
    time = System.nanoTime()
    calculateDeterminant(matrix)
    println("Time without paraller: " + (System.nanoTime() - time).toString)
    time = System.nanoTime()
    calculateDeterminantParaller(matrix)
    println("Time with paraller:    " + (System.nanoTime() - time).toString)

    println("\nTests on task 3 - sum elements in the array")
    println("Array of length 10")
    var arr = createArray(10)
    time = System.nanoTime()
    sumArray(arr)
    println("Time without paraller: " + (System.nanoTime() - time).toString)
    time = System.nanoTime()
    sumArrayParaller(arr)
    println("Time with paraller:    " + (System.nanoTime() - time).toString)

    println("\nArray of length 100")
    arr = createArray(100)
    time = System.nanoTime()
    sumArray(arr)
    println("Time without paraller: " + (System.nanoTime() - time).toString)
    time = System.nanoTime()
    sumArrayParaller(arr)
    println("Time with paraller:    " + (System.nanoTime() - time).toString)

    println("\nArray of length 1000")
    arr = createArray(1000)
    time = System.nanoTime()
    sumArray(arr)
    println("Time without paraller: " + (System.nanoTime() - time).toString)
    time = System.nanoTime()
    sumArrayParaller(arr)
    println("Time with paraller:    " + (System.nanoTime() - time).toString)

    println("\nArray of length 10000")
    arr = createArray(10000)
    time = System.nanoTime()
    sumArray(arr)
    println("Time without paraller: " + (System.nanoTime() - time).toString)
    time = System.nanoTime()
    sumArrayParaller(arr)
    println("Time with paraller:    " + (System.nanoTime() - time).toString)

    println("\nArray of length 100000")
    arr = createArray(100000)
    time = System.nanoTime()
    sumArray(arr)
    println("Time without paraller: " + (System.nanoTime() - time).toString)
    time = System.nanoTime()
    sumArrayParaller(arr)
    println("Time with paraller:    " + (System.nanoTime() - time).toString)

    println("\nArray of length 1000000")
    arr = createArray(1000000)
    time = System.nanoTime()
    sumArray(arr)
    println("Time without paraller: " + (System.nanoTime() - time).toString)
    time = System.nanoTime()
    sumArrayParaller(arr)
    println("Time with paraller:    " + (System.nanoTime() - time).toString)

    println("\nArray of length 10000000")
    arr = createArray(10000000)
    time = System.nanoTime()
    sumArray(arr)
    println("Time without paraller: " + (System.nanoTime() - time).toString)
    time = System.nanoTime()
    sumArrayParaller(arr)
    println("Time with paraller:    " + (System.nanoTime() - time).toString)
  }
}
