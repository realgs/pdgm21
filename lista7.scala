import scala.concurrent.Await
import scala.concurrent.Future
import scala.concurrent.duration.Duration
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.Random

object Lista7 {
  def main(args: Array[String]): Unit = {

    println("SUMMING TWO MATRIXES")
    val m1 = createMatrix(10, 10)
    val m2 = createMatrix(10, 10)
    var startTime = System.nanoTime()
    val sum1 = sumMatrixes(m1, m2)
    println("10x10 - time without paraller: " + (System.nanoTime() - startTime).toString)
    startTime = System.nanoTime()
    val parSum1 = parSumMatrixes(m1, m2)
    println("10x10 - time with paraller:    " + (System.nanoTime() - startTime).toString)

    val m3 = createMatrix(100, 100)
    val m4 = createMatrix(100, 100)
    startTime = System.nanoTime()
    val sum2 = sumMatrixes(m3, m4)
    println("100x100 - time without paraller: " + (System.nanoTime() - startTime).toString)
    startTime = System.nanoTime()
    val parSum2 = parSumMatrixes(m3, m4)
    println("100x100 - time with paraller:    " + (System.nanoTime() - startTime).toString)

    val m5 = createMatrix(1000, 1000)
    val m6 = createMatrix(1000, 1000)
    startTime = System.nanoTime()
    val sum3 = sumMatrixes(m5, m6)
    println("1000x1000 - time without paraller: " + (System.nanoTime() - startTime).toString)
    startTime = System.nanoTime()
    val parSum3 = parSumMatrixes(m5, m6)
    println("1000x1000 - time with paraller:    " + (System.nanoTime() - startTime).toString)

    val m7 = createMatrix(10000, 10000)
    val m8 = createMatrix(10000, 10000)
    startTime = System.nanoTime()
    val sum4 = sumMatrixes(m7, m8)
    println("10000x10000 - time without paraller: " + (System.nanoTime() - startTime).toString)
    startTime = System.nanoTime()
    val parSum4 = parSumMatrixes(m7, m8)
    println("10000x10000 - time with paraller:    " + (System.nanoTime() - startTime).toString)


    println("\nSUMMING ELEMENTS IN A BINARY TREE")
    val t1 = createTree(8)
    startTime = System.nanoTime()
    val sum1_ = sumBT(t1)
    println("depth = 8 - time without paraller: " + (System.nanoTime() - startTime).toString)
    startTime = System.nanoTime()
    val parSum1_ = parSumBT(t1)
    println("depth = 8 - time with paraller:    " + (System.nanoTime() - startTime).toString)

    val t2 = createTree(13)
    startTime = System.nanoTime()
    val sum2_ = sumBT(t2)
    println("depth = 13 - time without paraller: " + (System.nanoTime() - startTime).toString)
    startTime = System.nanoTime()
    val parSum2_ = parSumBT(t2)
    println("depth = 13 - time with paraller:    " + (System.nanoTime() - startTime).toString)

    val t3 = createTree(18)
    startTime = System.nanoTime()
    val sum3_ = sumBT(t3)
    println("depth = 18 - time without paraller: " + (System.nanoTime() - startTime).toString)
    startTime = System.nanoTime()
    val parSum3_ = parSumBT(t3)
    println("depth = 18 - time with paraller:    " + (System.nanoTime() - startTime).toString)

    val t4 = createTree(23)
    startTime = System.nanoTime()
    val sum4_ = sumBT(t4)
    println("depth = 23 - time without paraller: " + (System.nanoTime() - startTime).toString)
    startTime = System.nanoTime()
    val parSum4_ = parSumBT(t4)
    println("depth = 23 - time with paraller:    " + (System.nanoTime() - startTime).toString)
  }

  //-----------------------------------------------------------------------------------------------

  def createMatrix(n: Int, m: Int): Array[Array[Int]] =
    var matrix = Array.ofDim[Int](n, m)
    for (i <- 0 to n - 1)
      for (j <- 0 to m - 1)
        matrix(i)(j) = Random.nextInt(100)
    return matrix

  def sumMatrixes(matrix1: Array[Array[Int]], matrix2: Array[Array[Int]]): Int =
    var sum = 0
    for (i <- 0 to matrix1.length - 1)
      for (j <- 0 to matrix1(0).asInstanceOf[Array[Int]].length - 1)
        sum += matrix1(i)(j) + matrix2(i)(j)
    return sum

  def parSumMatrixes(matrix1: Array[Array[Int]], matrix2: Array[Array[Int]]): Int =
    def parSumMatrixesInner(matrix1: Array[Array[Int]], matrix2: Array[Array[Int]], rowStart: Int, rowEnd: Int): Int =
      var sum = 0
      for (i <- rowStart to rowEnd - 1)
        for (j <- 0 to matrix1(0).asInstanceOf[Array[Int]].length - 1)
          sum += matrix1(i)(j) + matrix2(i)(j)
      return sum

    val f1 = Future { parSumMatrixesInner(matrix1, matrix2, 0, matrix1.length / 6) }
    val f2 = Future { parSumMatrixesInner(matrix1, matrix2, matrix1.length / 6, matrix1.length * 2 / 6) }
    val f3 = Future { parSumMatrixesInner(matrix1, matrix2, matrix1.length * 2 / 6, matrix1.length * 3 / 6) }
    val f4 = Future { parSumMatrixesInner(matrix1, matrix2, matrix1.length * 3 / 6, matrix1.length * 4 / 6) }
    val f5 = Future { parSumMatrixesInner(matrix1, matrix2, matrix1.length * 4 / 6, matrix1.length * 5 / 6) }
    val f6 = Future { parSumMatrixesInner(matrix1, matrix2, matrix1.length * 5 / 6, matrix1.length) }

    return Await.result(f1, Duration.Inf) + Await.result(f2, Duration.Inf) + Await.result(f3, Duration.Inf)
      + Await.result(f4, Duration.Inf) + Await.result(f5, Duration.Inf) + Await.result(f6, Duration.Inf)

  //-----------------------------------------------------------------------------------------------

  sealed trait BT[+A]
  case object Empty extends BT[Nothing]
  case class Node[+A](elem: A, left: BT[A], right: BT[A]) extends BT[A]

  def createTree(n: Int): BT[Int] =
    if n > 0 then Node(Random.nextInt(100), createTree(n - 1), createTree(n - 1))
    else Empty

  def sumBT(tree: BT[Int]): Int =
    tree match
      case Empty => 0
      case Node(v, left, right) => v + sumBT(left) + sumBT(right)

  def parSumBT(tree: BT[Int]): Int =
    tree match
      case Empty => 0
      case Node(v, left, right) =>
        val sumLeft = Future { sumBT(left) }
        val sumRight = Future { sumBT(right) }
        v + Await.result(sumLeft, Duration.Inf) + Await.result(sumRight, Duration.Inf)

}

/*
SUMMING TWO MATRIXES
10x10 - time without paraller: 4200
10x10 - time with paraller:    119200
100x100 - time without paraller: 5100
100x100 - time with paraller:    49700
1000x1000 - time without paraller: 644000
1000x1000 - time with paraller:    500600
10000x10000 - time without paraller: 54382200
10000x10000 - time with paraller:    30453000

SUMMING ELEMENTS IN A BINARY TREE
depth = 8 - time without paraller: 128401
depth = 8 - time with paraller:    10786100
depth = 13 - time without paraller: 159099
depth = 13 - time with paraller:    1430900
depth = 18 - time without paraller: 3162300
depth = 18 - time with paraller:    1268301
depth = 23 - time without paraller: 173967800
depth = 23 - time with paraller:    92269799
*/
