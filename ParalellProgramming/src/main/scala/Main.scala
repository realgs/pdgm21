import scala.util.Random

object Main:
    def main(args: Array[String]): Unit =
        val matrix1 = Array.ofDim[Int](1500, 500)
        val matrix2 = Array.ofDim[Int](500, 2000)

        for (i <- matrix1.indices)
            for (j <- matrix1(0).indices)
                matrix1(i)(j) = i + j

        for (i <- matrix2.indices)
            for (j <- matrix2(0).indices)
                matrix2(i)(j) = i + j

        val multiplicator = new MatrixMultiplicator(matrix1, matrix2)

        println(" ==== Measuring matrix multiplicator ==== ")
        var start = System.currentTimeMillis()
        multiplicator.multiply()
        println(s"Single thread result: ${System.currentTimeMillis() - start}ms")

        start = System.currentTimeMillis()
        multiplicator.multiplyParallel()
        println(s"Dual thread result: ${System.currentTimeMillis() - start}ms")

        val array = Array.ofDim[Int](10*1000*1000)
        val r = new Random()
        for (i <- array.indices)
            array(i) = r.nextInt()

        val quicksorter = new Quicksorter()

        println(" ==== Measuring quicksorter ==== ")
        start = System.currentTimeMillis()
        quicksorter.quicksort(array)
        println(s"Single thread result: ${System.currentTimeMillis() - start}ms")

        for (i <- array.indices)
            array(i) = r.nextInt()

        start = System.currentTimeMillis()
        quicksorter.quicksortParalell(array)
        println(s"Dual thread result: ${System.currentTimeMillis() - start}ms")


