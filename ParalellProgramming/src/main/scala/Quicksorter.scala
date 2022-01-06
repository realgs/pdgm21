import scala.concurrent.{Await, Future}
import scala.concurrent.duration.Duration
import concurrent.ExecutionContext.Implicits.global
import scala.util.Random

class Quicksorter:
    val r = new Random

    private def partition(arr: Array[Int], begin: Int, end: Int): Int =
        val pivot = arr(begin + r.nextInt(end-begin))
        var i = begin - 1
        for (j <- begin until end)
            if arr(j) <= pivot then
                i += 1
                val swapTemp = arr(i)
                arr(i) = arr(j)
                arr(j) = swapTemp
        val swapTemp = arr(i + 1)
        arr(i + 1) = arr(end)
        arr(end) = swapTemp
        i + 1

    private def sort(arr: Array[Int], begin: Int, end: Int): Unit =
        if begin < end then
            val partitionIndex = partition(arr, begin, end)
            sort(arr, begin, partitionIndex - 1)
            sort(arr, partitionIndex + 1, end)

    def quicksort(arr: Array[Int]): Unit =
        sort(arr, 0, arr.length-1)

    def quicksortParalell(arr: Array[Int]): Unit =
            val partitionIndex = partition(arr, 0, arr.length-1)
            val f1 = Future {
                sort(arr, 0, partitionIndex - 1)
            }
            val f2 = Future {
                sort(arr, partitionIndex + 1, arr.length-1)
            }
            Await.result(f1, Duration.Inf)
            Await.result(f2, Duration.Inf)
