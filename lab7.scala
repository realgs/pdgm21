import scala.util.Random
import scala.concurrent.Await
import scala.concurrent.Future
import scala.concurrent.duration.Duration
import scala.concurrent.ExecutionContext.Implicits.global
import scala.language.postfixOps

object lab7:
  def genRandomArray(size: Int, limit: Int): Array[Int] =
    val arr = new Array[Int](size)
    for (i <- 0 until size)
      arr(i) = Random.nextInt(limit)
    arr

  //Sum of fibs of array
  def fibTail (x: Int) : Int =
    if x < 0 then throw new Exception(s"Nieprawidłowy argument: $x")
    def fibTailI (x: Int, fib1: Int, fib2: Int) : Int =
      x match
        case 0 => fib1
        case 1 => fib2
        case _ => fibTailI(x-1, fib2, fib1 + fib2)
    fibTailI(x, 0, 1)

  def sumFibsOfArray(arr: Array[Int]): Int =
    var sum = 0
    for (i <- 0 until arr.length)
      sum += fibTail(arr(i))
    sum

  def sumFibsOfArrayFuture(arr: Array[Int]): Int =
    def sumFibsOfArrayFutureI(beg: Int, end: Int): Int =
      var sum = 0
      for (i <- beg until end)
        sum += fibTail(arr(i))
      sum

    val len = arr.length

    if len < 1000000 then sumFibsOfArray(arr)
    else
      val f1 = Future{ sumFibsOfArrayFutureI(0, len/4) }
      val f2 = Future{ sumFibsOfArrayFutureI(len/4, len/2) }
      val f3 = Future{ sumFibsOfArrayFutureI(len/2, len*3/4) }
      val f4 = Future{ sumFibsOfArrayFutureI(len*3/4, len) }

      Await.result(f1, Duration.Inf) + Await.result(f2, Duration.Inf) + Await.result(f3, Duration.Inf) + Await.result(f4, Duration.Inf)

  //Quicksort
  def quickSort(arr: Array[Int]): Array[Int] =
    if arr.length <= 1 then arr
    else
      val pivot = arr(arr.length / 2)
      Array.concat(
        quickSort(arr filter (pivot >)),
        arr filter (pivot ==),
        quickSort(arr filter (pivot <)))

  def quickSortFuture(arr: Array[Int]): Array[Int] =
    if arr.length <= 1 then arr
    else
      val pivot = arr(arr.length / 2)

      val f1 = Future{ quickSort(arr filter (pivot >)) }
      val f2 = Future{ quickSort(arr filter (pivot <)) }

      Array.concat(
        Await.result(f1, Duration.Inf),
        arr filter (pivot ==),
        Await.result(f2, Duration.Inf)
      )

  def main(args: Array[String]): Unit =
    val arr1 = genRandomArray(100000, 15)

    val time1 = System.currentTimeMillis()
    println(sumFibsOfArray(arr1))
    val time2 = System.currentTimeMillis()
    println("Sequential SumOfFibs: " + (time2 - time1))

    val time3 = System.currentTimeMillis()
    println(sumFibsOfArrayFuture(arr1))
    val time4 = System.currentTimeMillis()
    println("Parallel SumOfFibs: " + (time4 - time3))

//    Size: 100000
//    Sequential SumOfFibs: 8
//    Parallel SumOfFibs: 79

//    Size: 1000000
//    Sequential SumOfFibs: 27
//    Parallel SumOfFibs: 87

//    Size: 10000000
//    Sequential SumOfFibs: 208
//    Parallel SumOfFibs: 107

//    Size: 100000000
//    Sequential SumOfFibs: 1552
//    Parallel SumOfFibs: 540

    println()

    println("Size: " + arr1.length)
    val time5 = System.currentTimeMillis()
//    println(quickSort(arr1).toList)
    quickSort(arr1)
    val time6 = System.currentTimeMillis()
    println("Sequential QS: " + (time6 - time5))

    val time7 = System.currentTimeMillis()
//    println(quickSortFuture(arr1).toList)
    quickSortFuture(arr1)
    val time8 = System.currentTimeMillis()
    println("Parallel QS: " + (time8 - time7))

//    Size: 100000
//    Sequential QS: 59
//    Parallel QS: 14

//    Size: 1000000
//    Sequential QS: 205
//    Parallel QS: 154

//    Size: 10000000
//    Sequential QS: 1457
//    Parallel QS: 861
