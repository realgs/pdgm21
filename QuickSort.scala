package QuickSort

import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import ParallelFunction.ParallelFunction.*

object QuickSort {

  private def swap(tab: Array[Int])(i: Int)(j: Int): Unit =
    val aux = tab(i)
    tab(i) = tab(j)
    tab(j) = aux

  private def choose_pivot(tab: Array[Int])(m:Int)(n:Int): Int =
    tab((m+n)/2)

  private def partition(tab: Array[Int])(l:Int)(r:Int): (Int,Int) =
    var i = l
    var j = r
    val pivot = choose_pivot(tab)(l)(r)
    while (i <= j)
      while (tab(i) < pivot) i += 1
      while (pivot < tab(j)) j -= 1
      if (i <= j) then
        swap(tab)(i)(j)
        i += 1
        j -= 1
      else ()
    (i,j)

  private def quick(tab: Array[Int])(l: Int)(r: Int): Unit =
    if l < r then
      val (i,j) = partition(tab)(l)(r)
      if j-l < r-i then
        quick(tab)(l)(j)
        quick(tab)(i)(r)
      else
        quick(tab)(i)(r)
        quick(tab)(l)(j)

  private def quickFuture(tab: Array[Int])(l: Int)(r: Int): Unit =
    if l < r then
      val (i,j) = partition(tab)(l)(r)
      if j-l < r-i then
        val f1 = Future(quick(tab)(l)(j))
        val f2 = Future(quick(tab)(i)(r))
        Await.result(f1, Duration.Inf)
        Await.result(f2, Duration.Inf)
      else
        val f1 = Future(quick(tab)(i)(r))
        val f2 = Future(quick(tab)(l)(j))
        Await.result(f1, Duration.Inf)
        Await.result(f2, Duration.Inf)

  private def quickParallel(tab: Array[Int])(l: Int)(r: Int): Unit =
    if l < r then
      val (i,j) = partition(tab)(l)(r)
      if j-l < r-i then parallel(quick(tab)(l)(j), quick(tab)(i)(r))
      else parallel(quick(tab)(i)(r), quick(tab)(l)(j))


  def quicksort(tab: Array[Int]): Unit =
    quick(tab)(0)(tab.length-1)

  def quicksortFuture(tab: Array[Int]): Unit =
    quickFuture(tab)(0)(tab.length-1)

  def quicksortParallel(tab: Array[Int]): Unit =
    quickParallel(tab)(0)(tab.length-1)

}
