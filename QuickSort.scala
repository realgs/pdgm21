import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.Random
import ParallelComponents.*

object QuickSort
{
  private val generateRandom = Random

  def printArray[A](arrayToPrint: Array[A]): Unit =
    println(arrayToPrint.mkString(" "))

  def generateArray(length: Int, minValue: Int, maxValue: Int): Array[Int] =
    Array.fill(length)(generateRandom.between(minValue, maxValue))

  def swap(tab: Array[Int])(i: Int)(j: Int): Unit =
    val aux = tab(i)
    tab(i) = tab(j)
    tab(j) = aux

  def choosePivot(tab: Array[Int])(m: Int)(n: Int): Int =
    tab((m + n) / 2)

  def partition(tab: Array[Int])(l: Int)(r: Int): (Int, Int) =
    var i = l
    var j = r
    val pivot = choosePivot(tab)(l)(r)
    while (i <= j) do
      while tab(i) < pivot do i += 1
      while pivot < tab(j) do j -= 1
      if i <= j then
        swap(tab)(i)(j)
        i += 1
        j -= 1
    (i, j)

  def quick(tab: Array[Int])(l: Int)(r: Int): Unit =
    if l < r then
      val (i: Int, j: Int) = partition(tab)(l)(r)
      if j - l < r - i
      then
        quick(tab)(l)(j)
        quick(tab)(i)(r)
      else
        quick(tab)(i)(r)
        quick(tab)(l)(j)
    else ()

  def quicksort(tab: Array[Int]): Unit =
    quick(tab)(0)(tab.length - 1)

  def quickAlwaysFuture(tab: Array[Int])(l: Int)(r: Int): Unit =
    if l < r then
      val (i: Int, j: Int) = partition(tab)(l)(r)
      if j - l < r - i
      then
        val f1 = Future(quickAlwaysFuture(tab)(l)(j))
        val f2 = Future(quickAlwaysFuture(tab)(i)(r))
        ParallelComponents.parallel(f1, f2)
      else
        val f1 = Future(quickAlwaysFuture(tab)(i)(r))
        val f2 = Future(quickAlwaysFuture(tab)(l)(j))
        ParallelComponents.parallel(f1, f2)
    else ()

  def quicksortAlwaysFuture(tab: Array[Int]): Unit =
    quickAlwaysFuture(tab)(0)(tab.length - 1)

  def quickMixedFuture(tab: Array[Int])(l: Int)(r: Int): Unit =
    if l < r then
      val (i: Int, j: Int) = partition(tab)(l)(r)
      if j - l < r - i
      then
        val f1 = Future(quick(tab)(l)(j))
        val f2 = Future(quick(tab)(i)(r))
        ParallelComponents.parallel(f1, f2)
      else
        val f1 = Future(quick(tab)(i)(r))
        val f2 = Future(quick(tab)(l)(j))
        ParallelComponents.parallel(f1, f2)
    else ()

  def quicksortMixedFuture(tab: Array[Int]): Unit =
    quickMixedFuture(tab)(0)(tab.length - 1)

  def quickMixedParallel(tab: Array[Int])(l: Int)(r: Int): Unit =
    if l < r then
      val (i: Int, j: Int) = partition(tab)(l)(r)
      if j - l < r - i
      then
        ParallelComponents.parallelName(quick(tab)(l)(j), quick(tab)(i)(r))
      else
        ParallelComponents.parallelName(quick(tab)(i)(r), quick(tab)(l)(j))
    else ()

  def quicksortMixedParallel(tab: Array[Int]): Unit =
    quickMixedParallel(tab)(0)(tab.length - 1)
}
