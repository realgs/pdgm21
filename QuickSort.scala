import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.*
import scala.util.Random

object QuickSort {
  
  def printArray[A](array: Array[A]): Unit =
    println(array.mkString(", "))

  def swap(tab: Array[Int])(i: Int)(j: Int): Unit =
    val aux = tab(i); tab(i) = tab(j); tab(j) = aux

  def partition(tab: Array[Int])(l: Int)(r: Int): (Int, Int) =
    var i = l; var j = r; val pivot = tab((i + j)/2)
    while i <= j do
      while tab(i) < pivot do i = i + 1
      while tab(j) > pivot do j = j - 1
      if i <= j
      then swap(tab)(i)(j); i = i + 1; j = j - 1
    (i, j)

  def parallel[A, B](taskA: Future[A], taskB: Future[B]): (A, B) =
    val result =
      for
        r1 <- taskA
        r2 <- taskB
      yield
        (r1, r2)
    Await.result(result, Duration.Inf)

  def parallel2[A, B](taskA: => A, taskB: => B): (A,B) =
    val future: Future[B] = Future { taskB }
    val a: A = taskA
    val b: B = Await.result(future, Duration.Inf)
    (a,b)


  
  def quicksort(tab: Array[Int]): Unit =
    quick(tab)(0)(tab.length - 1)

  def quick(tab: Array[Int])(l: Int)(r: Int) : Unit =
    if l < r then
      val (i, j) = partition(tab)(l)(r)
      if j-l < r-i then
        quick(tab)(l)(j)
        quick(tab)(i)(r)
      else
        quick(tab)(i)(r)
        quick(tab)(l)(j)
  
  
  
  def futureQuicksort(tab: Array[Int]): Unit =
    quickFuture(tab)(0)(tab.length - 1)

  def quickFuture(tab: Array[Int])(l: Int)(r: Int) : Unit =
    if l < r then
      val (i, j) = partition(tab)(l)(r)
      if j-l < r-i then
        val left = Future(quickFuture(tab)(l)(j))
        val right = Future(quickFuture(tab)(i)(r))
        parallel(left, right)
      else
        val left = Future(quickFuture(tab)(i)(r))
        val right = Future(quickFuture(tab)(l)(j))
        parallel(left, right)


  def futureQuicksort2(tab: Array[Int]): Unit =
    quickFuture2(tab)(0)(tab.length - 1)

  def quickFuture2(tab: Array[Int])(l: Int)(r: Int) : Unit =
    if l < r then
      val (i, j) = partition(tab)(l)(r)
      if j-l < r-i then
        val left = Future(quick(tab)(l)(j))
        val right = Future(quick(tab)(i)(r))
        Await.result(left, Duration.Inf)
        Await.result(right, Duration.Inf)
      else
        val left = Future(quick(tab)(i)(r))
        val right = Future(quick(tab)(l)(j))
        Await.result(left, Duration.Inf)
        Await.result(right, Duration.Inf)


  def parallelQuicksort(tab: Array[Int]): Unit =
    quickParallel(tab)(0)(tab.length - 1)

  def quickParallel(tab: Array[Int])(l: Int)(r: Int) : Unit =
    if l < r then
      val (i, j) = partition(tab)(l)(r)
      if j-l < r-i then
        parallel2(quick(tab)(l)(j), quick(tab)(i)(r))
      else
        parallel2(quick(tab)(i)(r), quick(tab)(l)(j))
}

