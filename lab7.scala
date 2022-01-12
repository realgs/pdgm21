import scala.concurrent.Await
import scala.concurrent.Future
import scala.concurrent.duration.Duration
import scala.concurrent.ExecutionContext.Implicits.global

object Main extends App
{
  //SUMOWANIE TABLICY 1D
  def createArray(length: Int): Array[Int] =
    var array = Array.ofDim[Int](length)

    for(i <- 0 to length - 1)
      array(i) = 100

    return array

  def sumArray(array: Array[Int]): Long =
    var sum = 0L

    for (i <- 0 to array.length - 1)
      sum += array(i)

    return sum

  def sumArrayParalell(array: Array[Int]): Long =
    def sumArrayInnerParallel(array: Array[Int], startingIndex: Int, endingIndex: Int): Long =
      var sum = 0L

      for (i <- startingIndex to endingIndex - 1)
        sum += array(i)

      return sum

    val f1 = Future{sumArrayInnerParallel(array, 0, array.length / 3)}
    val f2 = Future{sumArrayInnerParallel(array, array.length / 3, (2 * array.length) / 3)}
    val f3 = Future{sumArrayInnerParallel(array, (2 * array.length) / 3, array.length)}

    return Await.result(f1, Duration.Inf) + Await.result(f2, Duration.Inf) + Await.result(f3, Duration.Inf)

  //QUICK SORT
  def swap(array: Array[Int], i: Int, j: Int) =
    val temp = array(i)
    array(i) = array(j)
    array(j) = temp

  def choosePivot(array: Array[Int], i: Int, j: Int) =
    array((i + j) / 2)

  def partition(array: Array[Int], l: Int, r: Int) =
    var i = l
    var j = r
    val pivot = choosePivot(array, l, r)

    while i <= j do
      while array(i) < pivot do i += 1
      while pivot < array(j) do j -= 1
      if i <= j then
        swap(array, i, j)
        i += 1
        j -= 1

    (i, j)

  def quick(array: Array[Int], l: Int, r: Int): Unit =
    if l < r then
      val (i, j) = partition(array, l, r)

      if (j - l) < (r - i) then
        quick(array, l, j)
        quick(array, i, r)
      else
        quick(array, i, r)
        quick(array, l, j)
    else ()

  def paralellQuick(array: Array[Int], l: Int, r: Int): Unit =
    if l < r then
      val (i, j) = partition(array, l, r)

      if (j - l) < (r - i) then
        val left = Future(paralellQuick(array, l, j))
        var right = Future(paralellQuick(array, i, r))
        Await.result(left, Duration.Inf)
        Await.result(right, Duration.Inf)
      else
        val right = Future(paralellQuick(array, i, r))
        val left = Future(paralellQuick(array, l, j))
        Await.result(right, Duration.Inf)
        Await.result(left, Duration.Inf)
    else ()

  def quickSort(array: Array[Int]) =
    quick(array, 0, array.length - 1)

  def parallelQuickSort(array: Array[Int]) =
    paralellQuick(array, 0, array.length - 1)

  val r = scala.util.Random

  def createRandomArray(length: Int): Array[Int] =
    var array = Array.ofDim[Int](length)

    for(i <- 0 to length - 1)
      array(i) = r.nextInt(100000)

    return array

  var startTime = 0L
  var endTime = 0L

  //FIBONACCI
  def fib(n: Int): Long =
    n match
      case 0 => 0L
      case 1 => 1L
      case 2 => 1L
      case n => fib(n - 1) + fib(n - 2)

  def fibParallel(n: Int): Long =
    n match
      case 0 => 0L
      case 1 => 1L
      case 2 => 1L
      case n =>
        if n % 2 == 0 then
          val f1 = Future{fibParallel(n - 1)}
          val f2 = Future{fibParallel(n - 2)}

          Await.result(f1, Duration.Inf) + Await.result(f2, Duration.Inf)
        else
          fib(n - 1) + fib(n - 2)

  println("----------------------")

  //SUM OF ARRAY TESTS
  //TEST FOR 1000 ELEMENTS IN ARRAY
  startTime = System.nanoTime
  println("Sum of 1000 elements REGULAR: " + sumArray(createArray(1000)))
  endTime = System.nanoTime
  println("Elapsed time REGULAR: " + ((endTime - startTime) / 1e9d) + "s")
  startTime = System.nanoTime
  println("Sum of 1000 elements PARALLEL: " + sumArrayParalell(createArray(1000)))
  endTime = System.nanoTime
  println("Elapsed time PARALELL: " + ((endTime - startTime) / 1e9d) + "s")

  //TEST FOR 1000000 ELEMENTS IN ARRAY
  startTime = System.nanoTime
  println("\nSum of 1000000 elements REGULAR: " + sumArray(createArray(1000000)))
  endTime = System.nanoTime
  println("Elapsed time REGULAR: " + ((endTime - startTime) / 1e9d) + "s")
  startTime = System.nanoTime
  println("Sum of 1000000 elements PARALLEL: " + sumArrayParalell(createArray(1000000)))
  endTime = System.nanoTime
  println("Elapsed time PARALELL: " + ((endTime - startTime) / 1e9d) + "s")

  //TEST FOR 10000000 ELEMENTS IN ARRAY
  startTime = System.nanoTime
  println("\nSum of 10000000 elements REGULAR: " + sumArray(createArray(100000000)))
  endTime = System.nanoTime
  println("Elapsed time REGULAR: " + ((endTime - startTime) / 1e9d) + "s")
  startTime = System.nanoTime
  println("Sum of 10000000 elements PARALLEL: " + sumArrayParalell(createArray(100000000)))
  endTime = System.nanoTime
  println("Elapsed time PARALELL: " + ((endTime - startTime) / 1e9d) + "s")

  //QUICK SORT TESTS
  //TEST FOR 1000 ELEMENTS IN ARRAY
  println("\n\nTEST OF QUICK SORT")

  var randomArray = createRandomArray(1)

  println("\nQuicksort of 1000 elements...")
  randomArray = createRandomArray(1000)
  startTime = System.nanoTime
  quickSort(randomArray)
  endTime = System.nanoTime
  println("Elapsed time REGULAR: " + ((endTime - startTime) / 1e9d) + "s")
  startTime = System.nanoTime
  parallelQuickSort(randomArray)
  endTime = System.nanoTime
  println("Elapsed time PARALLEL: " + ((endTime - startTime) / 1e9d) + "s")

  //FIBONACCI TESTS
  println("\n\nFIBONACCI TESTS: ")

  //43 FIBONACCI ELEMENT
  startTime = System.nanoTime
  println("43 Fibonacci element REGULAR: " + fib(43))
  endTime = System.nanoTime
  println("Elapsed time REGULAR: " + ((endTime - startTime) / 1e9d) + "s")

  startTime = System.nanoTime
  println("43 Fibonacci element PARALLEL: " + fibParallel(43))
  endTime = System.nanoTime
  println("Elapsed time PARALLEL: " + ((endTime - startTime) / 1e9d) + "s")

  //45 FIBONACCI ELEMENT
  startTime = System.nanoTime
  println("\n45 Fibonacci element REGULAR: " + fib(46))
  endTime = System.nanoTime
  println("Elapsed time REGULAR: " + ((endTime - startTime) / 1e9d) + "s")

  startTime = System.nanoTime
  println("45 Fibonacci element PARALLEL: " + fibParallel(46))
  endTime = System.nanoTime
  println("Elapsed time PARALLEL: " + ((endTime - startTime) / 1e9d) + "s")
}
