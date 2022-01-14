import java.util
import scala.annotation.tailrec
import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.{Duration, DurationInt, SECONDS}
import scala.runtime.Nothing$
import scala.util.{Failure, Random, Success}

/**
 * @author Jakub Szwedowicz
 * @version 1.0
 *          date: 13.01.2022
 *          email: kuba.szwedowicz@gmail.com
 */
case object Algorithms:
  private val maxThreadCounter = 4

  case object Accumulator:

    def accumulate[A](array: Array[A], op: (A, A) => A, begin: A): A =
      accumulate(array, op, begin, 0, array.length)

    def parallelAccumulate[A](array: Array[A], op: (A, A) => A, beginValue: A): A =
      if (array.length <= 1e6) then accumulate(array, op, beginValue, 0, array.length)
      else
        val rangesIndices = Array(
          0
          , array.length / 4
          , 2 * (array.length / 4)
          , 3 * (array.length / 4)
          , array.length)

        val futures = (0 until maxThreadCounter).map(rangesIndex => Future {
          accumulate(array, op, beginValue, rangesIndices(rangesIndex), rangesIndices(rangesIndex + 1))
        })
        val result = Await.result(Future.sequence(futures), 5.second)
        result.foldLeft(beginValue)((m, n) => op(m, n))

    def testAccumulator(): Unit =
      val numbers = Array.fill(1e9.toInt)(Random.nextInt(1234))
      val addOp = (m: Int, n: Int) => m + n

      var start = System.nanoTime()
      println(Algorithms.Accumulator.accumulate(numbers, addOp, 0))
      var end = System.nanoTime()
      println("Accumulator.accumulate took: " + (end - start) / 1e6 + " [ms]")
      start = System.nanoTime()
      println(Algorithms.Accumulator.parallelAccumulate(numbers, addOp, 0))
      end = System.nanoTime()
      println("Accumulator.parallelAccumulate took: " + (end - start) / 1e6 + " [ms]")

    @tailrec
    private def accumulate[A](array: Array[A], op: (A, A) => A, res: A, inclusiveBegin: Int, exclusiveEnd: Int): A =
      if (inclusiveBegin == exclusiveEnd) then res
      else
        accumulate(array, op, op(res, array(inclusiveBegin)), inclusiveBegin + 1, exclusiveEnd)

  case object Sorter:
    def quickSort[A](array: Array[A], op: (A, A) => Int): Unit =
      quickSortHelper(array, op, 0, array.length - 1)

    def parallelQuickSort[A](array: Array[A], op: (A, A) => Int): Unit =
      def parallelQuickSortHelper(low: Int, high: Int): Unit =
        if (high - low < 1e4) quickSortHelper(array, op, low, high)
        else
          val index = partition(array, op, low, high)
          val future = List(
            Future {
              if (low < index - 1) then parallelQuickSortHelper(low, index - 1)
            },
            Future {
              if (high > index) then parallelQuickSortHelper(index, high)
            }
          )
          Await.ready(Future.sequence(future), 5.second)

      val res = Future.apply(parallelQuickSortHelper(0, array.length - 1))
      Await.ready(res, 5.second).onComplete(f => f match
        case Success(_) => None
        case Failure(ex) => {
          println("Future pool broken!")
          ex.printStackTrace()
        }
      )

    def testSorter(): Unit =
      val numbers1 = Array.fill(10e5.toInt)(Random.nextInt(1234))
      val numbers2 = Array.copyOf(numbers1, numbers1.length)

      val comparator = (m: Int, n: Int) => {
        val diff = m - n
        if (diff < 0) then -1
        else if (diff == 0) then 0
        else 1
      }

      var start = System.nanoTime()
      quickSort(numbers1, comparator)
      var end = System.nanoTime()
      println("Sorter.quickSort took: " + ((end - start) / 1e6) + " [ms]")
      if (!isSorted(numbers1, comparator)) throw new Exception("QuickSort failed to do the sorting!")

      start = System.nanoTime()
      parallelQuickSort(numbers2, comparator)
      end = System.nanoTime()
      println("Sorter.parallelQuickSort took: " + (end - start) / 1e6 + " [ms]")
      if (!isSorted(numbers2, comparator)) throw new Exception("QuickSort failed to do the sorting!")

    private def isSorted[A](array: Array[A], op: (A, A) => Int): Boolean =
      for (i <- 0 until array.length - 2) if (op(array(i), array(i + 1)) == 1) then return false
      true

    private def quickSortHelper[A](array: Array[A], op: (A, A) => Int, low: Int, high: Int): Unit =
      val index = partition(array, op, low, high)
      if (low < index - 1) then quickSortHelper(array, op, low, index - 1)
      if (high > index) then quickSortHelper(array, op, index, high)

    private def partition[A](array: Array[A], op: (A, A) => Int, begin: Int, end: Int): Int =
      val middle = (end + begin) / 2
      val pivot = array(middle)
      var i = begin
      var j = end
      while (i <= j)
        while (op(array(i), pivot) == -1) i = i + 1
        while (op(array(j), pivot) == 1) j = j - 1
        if (i <= j) then
          val temp = array(i)
          array(i) = array(j)
          array(j) = temp
          i = i + 1
          j = j - 1
      i