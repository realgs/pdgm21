package SumOfArray

import scala.concurrent.duration.Duration
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Await, Future}
import ParallelFunction.ParallelFunction.*

object SumOfArray {

  private def partialSum(begin: Int, end: Int, arr: Array[Int]): Int =
    var i = begin
    var result = 0

    while (i<end)
      {
        result += arr(i)
        i += 1
      }

    result

  def sum(arr: Array[Int]): Int =
    partialSum(0, arr.length, arr)

  def sumFuture(arr: Array[Int]): Int =
    val f1 = Future(partialSum(0, arr.length/2, arr))
    val f2 = Future(partialSum(arr.length/2, arr.length, arr))
    val res = for {
      x <- f1
      y <- f2
    } yield x+y
    Await.result(res, Duration.Inf)

  def sumParallel(arr: Array[Int]): Int =
    val (f1, f2) = parallel(partialSum(0, arr.length/2, arr), partialSum(arr.length/2, arr.length, arr))
    f1 + f2
}
