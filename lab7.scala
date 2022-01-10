import com.sun.org.apache.xalan.internal.lib.ExsltDatetime.time

import scala.annotation.tailrec
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.Random


object lab7 {

  def parallel[A, B](taskA: => A, taskB: => B): (A, B) =
    val futureA: Future[A] = Future {
      taskA
    }
    val futureB: Future[B] = Future {
      taskB
    }
    val resA: A = Await.result(futureA, Duration.Inf)
    val resB: B = Await.result(futureB, Duration.Inf)
    (resA, resB)


  def time[T](block: => T): Unit = {
    val start = System.nanoTime()
    val function = block
    val end = System.nanoTime()
    println(s"Time taken: ${(end - start)} ns")
  }

  private val r = Random

  def generateList(length: Int, minValue: Int, maxValue: Int): List[Int] =
    def generateList(length: Int, result: List[Int]): List[Int] =
      length match
        case 0 => result
        case _ => generateList(length - 1, r.between(minValue, maxValue) :: result)

    generateList(length, Nil)

  def isPrimeNumber(num: Long): Boolean =
    def isPrimeTail(num: Long, lastDigits: Long): Boolean =
      if (lastDigits == 1) true
      else if (num % lastDigits == 0) false
      else isPrimeTail(num, lastDigits - 1)

    if (num > 0) isPrimeTail(num, num - 1)
    else false

  def isPrimeNumberParallel(num: Long): Boolean =
    def isPrimeTail2(num: Long, lastDigits: Long): Boolean =
      if ((lastDigits == 1) || (lastDigits == 0)) true
      else if (num % lastDigits == 0) false
      else isPrimeTail2(num, lastDigits - 2)

    if (num < 0) false
    else
      val (fib1, fib2) = parallel(isPrimeTail2(num, num - 1), isPrimeTail2(num, num - 2))
      fib1 && fib2


  def swap[A](arr: Array[A], i: Int, j: Int): Unit =
    val tmp = arr(i)
    arr(i) = arr(j)
    arr(j) = tmp

  def partition(arr: Array[Int], left: Int, right: Int): (Int, Int) =
    var i = left
    var j = right
    val pivot = arr((i + j) / 2)
    while (i <= j) {
      while (arr(i) < pivot) {
        i += 1
      }
      while (arr(j) > pivot) {
        j -= 1
      }
      if i <= j then
        swap(arr, i, j)
        i += 1
        j -= 1
    }
    (i, j)

  def quickHelper(arr: Array[Int], left: Int, right: Int): Unit =
    if left < right then
      val (i, j) = partition(arr, left, right)
      if (j - left) < (right - i) then
        quickHelper(arr, left, j)
        quickHelper(arr, i, right)
      else
        quickHelper(arr, i, right)
        quickHelper(arr, left, j)

  def quickSort(arr: Array[Int]) =
    quickHelper(arr, 0, arr.length - 1)


  def quickSortParallelHelper(arr: Array[Int], left: Int, right: Int): Unit =
    if left < right then
      val (i, j) = partition(arr, left, right)
      if (j - left) < (right - i) then
        val futureLeft = Future {
          quickHelper(arr, left, j)
        }
        val futureRight = Future {
          quickHelper(arr, i, right)
        }
        parallel(futureLeft, futureRight)
      else
        val futureRight = Future {
          quickHelper(arr, i, right)
        }
        val futureLeft = Future {
          quickHelper(arr, left, j)
        }
        parallel(futureLeft, futureRight)

  def quickSortParallel(arr: Array[Int]) =
    quickSortParallelHelper(arr, 0, arr.length - 1)


  def splitPosition[A](list: List[A], position: Int): (List[A], List[A]) =
    @tailrec
    def splitPositionHelper(list: List[A], pos: Int, result: (List[A], List[A])): (List[A], List[A]) =
      (pos, list) match
        case (0, list) => (result._1, list)
        case (_, Nil) => (result._1, Nil)
        case (_, head :: tail) => splitPositionHelper(tail, pos - 1, (head :: result._1, Nil))

    splitPositionHelper(list, position, (Nil, Nil))


  def merge(list1: List[Int], list2: List[Int]): List[Int] =
    def mergeHelper(list1: List[Int], list2: List[Int], result: List[Int]): List[Int] =
      (list1, list2) match
        case (h1 :: t1, h2 :: t2) => if h1 < h2 then mergeHelper(t1, list2, h1 :: result) else mergeHelper(list1, t2, h2 :: result)
        case (h1 :: t1, Nil) => mergeHelper(t1, list2, h1 :: result)
        case (Nil, h2 :: t2) => mergeHelper(list1, t2, h2 :: result)
        case (Nil, Nil) => result

    mergeHelper(list1, list2, Nil).reverse


  def mergeSort(listToSort: List[Int]): List[Int] =
    if listToSort.length < 2 then listToSort
    else
      val (list1, list2) = splitPosition(listToSort, listToSort.length / 2)
      merge(mergeSort(list1), mergeSort(list2))


  def mergeSortParallel(listToSort: List[Int], maxDepth: Int): List[Int] =
    if maxDepth <= 0 then mergeSort(listToSort)
    else if listToSort.length < 2 then listToSort
    else
      val (list1, list2) = splitPosition(listToSort, listToSort.length / 2)
      val (result1, result2) = parallel(mergeSortParallel(list1, maxDepth - 1), mergeSortParallel(list2, maxDepth - 1))
      merge(result1, result2)


  def main(args: Array[String]): Unit = {

    val tenThousandElements = generateList(10000, 1, 20000)
    val hundredThousandElements = generateList(100000, 1, 2000000)
    val millionElements = generateList(1000000, 1, 20000000)
    val tenMillionElements = generateList(10000000, 1, 20000000)

    time {
      isPrimeNumber(1234) // Time taken: 31900 ns
    }

    time {
      isPrimeNumberParallel(1234) // Time taken: 104560500 ns
    }

    time {
      isPrimeNumber(123456789) // Time taken: 784688600 ns
    }

    time {
      isPrimeNumberParallel(123456789) // Time taken: 696697100 ns
    }

    time {
      isPrimeNumber(1234567890) // Time taken: 7317016700 ns
    }

    time {
      isPrimeNumberParallel(1234567890) // Time taken: 696697100 ns
    }

    //  Wniosek: Dla mniejszych liczb efektywniej jest użyć
    //    zwykłego algortymu. Dla większych n >= 10^6, należy
    //  użyć Parallel, który będzie efektywniejszy.


    // ######################### MERGESORT ############################
    time {
      mergeSort(tenThousandElements) // Time taken: 907312800 ns
    }

    time {
      mergeSort(hundredThousandElements) // Time taken: 249168800 ns
    }

    time {
      mergeSort(millionElements) // Time taken: 3993386600 ns
    }

    time {
      mergeSort(tenMillionElements) // Time taken: 40098882500 ns
    }

    //    //##################################################################


    // ##################### MERGESORT PARALLEL ########################
    time {
      mergeSortParallel(tenThousandElements, 4) // Time taken: 930835000 ns
    }

    time {
      mergeSortParallel(hundredThousandElements, 4) // Time taken: 185496800 ns
    }

    time {
      mergeSortParallel(millionElements, 4) // Time taken: 2121011400 ns
    }

    time {
      mergeSortParallel(tenMillionElements, 4) // Time taken: 38473768100 ns
    }

    //##################################################################


    //  Wniosek: Dla mniejszych liczb efektywniej jest użyć
    //     zwykłego algortymu. Dla większych n >= 10^6, należy
    //     użyć Parallel, który będzie efektywniejszy.


    // ######################### QUICKSORT ############################
    time {
      quickSort(tenThousandElements.toArray) // Time taken: 79491900 ns
    }

    time {
      quickSort(hundredThousandElements.toArray) // Time taken: 31603400 ns
    }

    time {
      quickSort(millionElements.toArray) // Time taken: 1158813900 ns
    }

    time {
      quickSort(tenMillionElements.toArray) // Time taken: 4528099500 ns
    }

    //    //##################################################################


    // ##################### QUICKSORT PARALLEL ########################
    time {
      quickSortParallel(tenThousandElements.toArray) // Time taken: 163732700 ns
    }

    time {
      quickSortParallel(hundredThousandElements.toArray) // Time taken: 15041600 ns
    }

    time {
      quickSortParallel(millionElements.toArray) // Time taken: 72745200 ns
    }

    time {
      quickSortParallel(tenMillionElements.toArray) // Time taken: 1668606500 ns
    }

    //##################################################################


  }
}
