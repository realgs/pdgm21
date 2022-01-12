import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}
import concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration.Inf
import scala.util.Random

object lab7 {

  // * * * FIBONACCI * * *

  def fibSequence(n: Int): Int =
    if (n < 0) throw new IllegalArgumentException("cannot generate negative Fibonacci number")
    if (n < 2)
      return 1
    return fibSequence(n - 1) + fibSequence(n - 2)

  def fibParallel(n: Int): Int =
    if (n < 0) throw new IllegalArgumentException("cannot generate negative Fibonacci number")
    if (n < 2)
      return 1
    val f1 = Future(fibSequence(n - 1))
    val f2 = Future(fibSequence(n - 2))
    return Await.result(f1, Inf) + Await.result(f2, Inf)

  // * * * MERGE SORT * * *

  def mergeSortSequence(arr: Array[Int]): Unit =
    merge_sort_sequence(arr, 0, arr.length - 1)

  def mergeSortParallel(arr: Array[Int]): Unit =
    merge_sort_parallel(arr, 0, arr.length - 1)

  def merge_sort_parallel(arr: Array[Int], left: Int, right: Int): Unit =
    if (left < right)
      var mid = (left + right) / 2
      Future(merge_sort_sequence(arr, left, mid))
      Future(merge_sort_sequence(arr, mid + 1, right))
      merge(arr, left, mid, right)

  def merge_sort_sequence(arr: Array[Int], left: Int, right: Int): Unit =
    if (left < right)
      var mid = (left + right) / 2
      merge_sort_sequence(arr, left, mid)
      merge_sort_sequence(arr, mid + 1, right)
      merge(arr, left, mid, right)

  def merge(arr: Array[Int], left: Int, mid: Int, right: Int): Unit =
    var temp = new Array[Int](right - left + 1)
    var i = left
    var j = mid + 1
    var k = 0

    while (i <= mid && j <= right)
      if (arr(i) <= arr(j))
        temp(k) = arr(i)
        k += 1
        i += 1
      else
        temp(k) = arr(j)
        k += 1
        j += 1

    while (i <= mid)
      temp(k) = arr(i)
      k += 1
      i += 1

    while (j <= right)
      temp(k) = arr(j)
      k += 1
      j += 1

    for (i <- left to right)
      arr(i) = temp(i - left)

  // * * * QUICKSORT * * *

  def swap(i: Int, j: Int, arr: Array[Int]): Unit = {
    val temp = arr(i)
    arr(i) = arr(j)
    arr(j) = temp
  }

  def partition(arr: Array[Int], left: Int, right: Int): Int = {
    val pivot = arr(right)
    var i = left - 1
    for (j <- left until right) {
      if (arr(j) < pivot) {
        i += 1
        swap(i, j, arr)
      }
    }
    swap(i + 1, right, arr)
    i + 1
  }

  def quickSequence(arr: Array[Int], left: Int, right: Int): Unit = {
    if (left < right) {
      val pi = partition(arr, left, right)
      quickSequence(arr, left, pi - 1)
      quickSequence(arr, pi + 1, right)
    }
  }

  def quickParallel(arr: Array[Int], left: Int, right: Int): Unit = {
    if (left < right) {
      val mid = partition(arr, left, right)
      Future(quickSequence(arr, left, mid - 1))
      Future(quickSequence(arr, mid + 1, right))
    }
  }

  def quickSortSequence(array: Array[Int]): Unit =
    quickSequence(array, 0, array.length - 1)

  def quickSortParallel(tab: Array[Int]): Unit =
    quickParallel(tab, 0, tab.length - 1)

  def main(args: Array[String]): Unit = {

    println("Fibonacci (30 elements) - sequence: ")
    timeMeasure(fibSequence(30))
    println("Fibonacci (30 elements) - parallel: ")
    timeMeasure(fibParallel(30))
    println("Fibonacci (35 elements) - sequence: ")
    timeMeasure(fibSequence(35))
    println("Fibonacci (35 elements) - parallel: ")
    timeMeasure(fibParallel(35))
    println("Fibonacci (40 elements) - sequence: ")
    timeMeasure(fibSequence(40))
    println("Fibonacci (40 elements) - parallel: ")
    timeMeasure(fibParallel(40))
    println("Fibonacci (45 elements) - sequence: ")
    timeMeasure(fibSequence(45))
    println("Fibonacci (45 elements) - parallel: ")
    timeMeasure(fibParallel(45))

    println(" * * * * * * * * * * * * * * * * * * * * * * * * * * * ")

    val arr1 = Array.fill(999)(Random.nextInt(999))
    val arr2 = Array.fill(9999)(Random.nextInt(9999))
    val arr3 = Array.fill(99999)(Random.nextInt(99999))

    println("Quick sort (999 elements) - sequence: ")
    timeMeasure(quickSortSequence(arr1.clone()))
    println("Quick sort (999 elements) - parallel: ")
    timeMeasure(quickSortParallel(arr1.clone()))
    println("Quick sort (9999 elements) - sequence: ")
    timeMeasure(quickSortSequence(arr2.clone()))
    println("Quick sort (9999 elements) - parallel: ")
    timeMeasure(quickSortParallel(arr2.clone()))
    println("Quick sort (99999 elements) - sequence: ")
    timeMeasure(quickSortSequence(arr3.clone()))
    println("Quick sort (99999 elements) - parallel: ")
    timeMeasure(quickSortParallel(arr3.clone()))

    println(" * * * * * * * * * * * * * * * * * * * * * * * * * * * ")

    println("Merge sort (999 elements) - sequence: ")
    timeMeasure(mergeSortSequence(arr1.clone()))
    println("Merge sort (999 elements) - parallel: ")
    timeMeasure(mergeSortParallel(arr1.clone()))
    println("Merge sort (9999 elements) - sequence: ")
    timeMeasure(mergeSortSequence(arr2.clone()))
    println("Merge sort (9999 elements) - parallel: ")
    timeMeasure(mergeSortParallel(arr2.clone()))
    println("Merge sort (99999 elements) - sequence: ")
    timeMeasure(mergeSortSequence(arr3.clone()))
    println("Merge sort (99999 elements) - parallel: ")
    timeMeasure(mergeSortParallel(arr3.clone()))

//    quickSortSequence(arr1)
//
//    for(i <- 0 until arr1.length)
//      println(arr1(i))

  }

  def timeMeasure[A](block: => A): Unit = {
    val start = System.nanoTime()
    block
    val end = System.nanoTime()
    println("Time: " + (end - start))
  }

}
