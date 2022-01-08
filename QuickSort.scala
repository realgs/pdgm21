import scala.concurrent.*
import scala.concurrent.duration.*
import ExecutionContext.Implicits.global
import scala.util.{Random, Success}
import Benchmark._

object QuickSort {

  def main(args: Array[String]): Unit =

    //results
    //parallel is faster from 1000 elements(sometimes 10000) in both low and high range
    //for small number of elements sequential programming is definitely better

    println("\nlow range (-100; 100)")
    println("---------------------------")
    var elementNr = 10
    while elementNr <= 1_000_000 do
      val listTest = generateListInt(elementNr, -100, 100)
      var arrayTest = listTest.toArray

      println(elementNr + " elements test")
      println("sequential:\t" + measureTime(quicksort(arrayTest)))
      arrayTest = listTest.toArray
      println("parallel:\t" + measureTime(quicksortParallel(arrayTest)))
      elementNr *= 10

    println("\nhigh range (-100_000; 100_000)")
    println("---------------------------")
    elementNr = 10
    while elementNr <= 1_000_000 do
      val listTest = generateListInt(elementNr, -100_000, 100_000)
      var arrayTest = listTest.toArray

      println(elementNr + " elements test")
      println("sequential:\t" + measureTime(quicksort(arrayTest)))
      arrayTest = listTest.toArray
      println("parallel:\t" + measureTime(quicksortParallel(arrayTest)))
      elementNr *= 10


  def swap[T](arr: Array[T], i: Int, j: Int): Unit =

    val buff = arr(i)
    arr(i) = arr(j)
    arr(j) = buff

  def choose_pivot[T](arr: Array[T], i: Int, j: Int): T =
    arr((i + j) / 2)

  def partition(arr: Array[Int], left: Int, right: Int): (Int, Int) =

    var i = left
    var j = right
    val pivot = choose_pivot(arr, left, right)

    while i <= j do
      while arr(i) < pivot do
        i += 1
      while pivot < arr(j) do
        j -= 1
      if (i <= j)
        swap(arr, i, j)
        i += 1
        j -= 1

    (i, j)

  def quick(arr: Array[Int], left: Int, right: Int): Unit =

    if(left < right)
      val (i, j) = partition(arr, left, right)
      if j - left < right - i then
        val _ = quick(arr, left, j)
        quick(arr, i, right)
      else
        val _ = quick(arr, i, right)
        quick(arr, left, j)

  def quickParallel(arr: Array[Int], left: Int, right: Int): Unit =

    if(left < right)
      val (i, j) = partition(arr, left, right)
      if j - left < right - i then
        val leftFuture = Future{quick(arr, left, j)}
        quick(arr, i, right)
        Await.result(leftFuture, Duration.Inf)
      else
        val rightFuture = Future{quick(arr, i, right)}
        quick(arr, left, j)
        Await.result(rightFuture, Duration.Inf)

  def quicksort(arr: Array[Int]): Unit =
    quick(arr, 0, arr.length - 1)

  def quicksortParallel(arr: Array[Int]): Unit =
    quickParallel(arr, 0, arr.length-1)
}
