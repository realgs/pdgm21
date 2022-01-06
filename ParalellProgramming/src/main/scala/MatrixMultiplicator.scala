import scala.concurrent.{Await, Future}
import scala.concurrent.duration.Duration
import concurrent.ExecutionContext.Implicits.global

class MatrixMultiplicator(matrix1: Array[Array[Int]], matrix2: Array[Array[Int]]):

    def multiply(): Array[Array[Int]] =
        val matrix1width = matrix1(0).length
        val matrix1height = matrix1.length
        val matrix2width = matrix2(0).length
        val matrix2height = matrix2.length
        if matrix1width != matrix2height then throw new Exception("Multiplying is impossible!")
        val result = Array.ofDim[Int](matrix1height, matrix2width)
        for (i <- 0 until matrix1height)
            for (j <- 0 until matrix2width)
                for (k <- 0 until matrix1width)
                    result(i)(j) += matrix1(i)(k) * matrix2(k)(j)
        result

    private def multiplyParallelHelper(matrix2widthBegin: Int, matrix2widthEnd: Int, result: Array[Array[Int]]): Unit =
        val st = System.currentTimeMillis()
        val matrix1width = matrix1(0).length
        val matrix1height = matrix1.length
        for (i <- 0 until matrix1height)
            for (j <- matrix2widthBegin until matrix2widthEnd)
                for (k <- 0 until matrix1width)
                    result(i)(j) += matrix1(i)(k) * matrix2(k)(j)

    def multiplyParallel(): Array[Array[Int]] =
        val matrix1width = matrix1(0).length
        val matrix1height = matrix1.length
        val matrix2width = matrix2(0).length
        val matrix2height = matrix2.length
        if matrix1width != matrix2height then throw new Exception("Multiplying is impossible!")
        val result = Array.ofDim[Int](matrix1height, matrix2width)
        val f1 = Future {
            multiplyParallelHelper(0, matrix2width / 2, result)
        }
        val f2 = Future {
            multiplyParallelHelper(matrix2width / 2, matrix2width, result)
        }
        Await.result(f1, Duration.Inf)
        Await.result(f2, Duration.Inf)
        result