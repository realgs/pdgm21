import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}

object Lista7 {

  def fib(n: Int): Int = {
    n match
      case 0 => 0
      case 1 => 1
      case _ => fib(n - 2) + fib(n - 1)
  }

  def fibParallel(n: Int): Int = {
    n match
      case 0 => 0
      case 1 => 1
      case _ =>
        val future1 = Future(fib(n - 2))
        val future2 = Future(fib(n - 1))
        val await1 = Await.result(future1, Duration.Inf)
        val await2 = Await.result(future2, Duration.Inf)
        await1 + await2
  }


  def swap[A](tab: Array[A])(i: Int)(j: Int) = {
    val aux = tab(i)
    tab(i) = tab(j)
    tab(j) = aux
  }


  def choose_pivot[A](tab: Array[A])(m: Int)(n: Int) = {
    tab((m + n) / 2)
  }


  def partition(tab: Array[Int])(l: Int)(r: Int) = {
    var i = l
    var j = r
    val pivot = choose_pivot(tab)(l)(r)
    while i <= j do
      while tab(i) < pivot do i += 1
      while pivot < tab(j) do j -= 1
      if (i <= j) then
        swap(tab)(i)(j)
        i += 1
        j -= 1
    (i, j)
  }


  def quick(tab: Array[Int])(l: Int)(r: Int): Unit = {
    if l < r then
      val (i, j) = partition(tab)(l)(r)
      if j - l < r - i then
        quick(tab)(l)(j)
        quick(tab)(i)(r)

      else
        quick(tab)(i)(r)
        quick(tab)(l)(j)
  }

  def quickParallel(tab: Array[Int])(l: Int)(r: Int): Unit = {
    if l < r then
      val (i, j) = partition(tab)(l)(r)
      if j - l < r - i then
        val future1 = Future(quick(tab)(l)(j))
        val future2 = Future(quick(tab)(i)(r))
        Await.result(future1, Duration.Inf)
        Await.result(future2, Duration.Inf)


      else
        val future1 = Future(quick(tab)(i)(r))
        val future2 = Future(quick(tab)(l)(j))
        Await.result(future1, Duration.Inf)
        Await.result(future2, Duration.Inf)
  }

  def quicksort(tab: Array[Int]) = {
    quick(tab)(0)(tab.length - 1)
  }

  def quicksortParallel(tab: Array[Int]) = {
    quick(tab)(0)(tab.length - 1)
  }

  def printArray(array: Array[Int]): Unit = {
    print('[')
    for (i <- 0 to array.length - 1)
      print(array(i) + " ")
    println(']')
  }

  def mergeSortParallel(seq: List[Int]): List[Int] = seq match {
    case Nil => Nil
    case xs::Nil => List(xs)
    case _ =>
      val (left, right) = seq splitAt seq.length/2
      val leftFuture = Future(mergeSort(left))
      val rightFuture = Future(mergeSort(right))
      val resultLeft = Await.result(leftFuture, Duration.Inf)
      val resultRight = Await.result(rightFuture, Duration.Inf)
      merge(resultLeft, resultRight)
  }

  def mergeSort(seq: List[Int]): List[Int] = seq match {
    case Nil => Nil
    case xs::Nil => List(xs)
    case _ =>
      val (left, right) = seq splitAt seq.length/2
      merge(mergeSort(left), mergeSort(right))
  }

  def merge(seq1: List[Int], seq2: List[Int]): List[Int] = {
    (seq1, seq2) match {
      case (Nil, _) => seq2
      case (_, Nil) => seq1
      case (x :: xs, y :: ys) =>
        if (x < y) x :: merge(xs, seq2)
        else y :: merge(seq1, ys)
    }
  }


  def main(args: Array[String]): Unit = {

    //testy dla QuickSort 100

    println("QuickSort 100 elementow")

    val t11 = Array.fill(100)(scala.util.Random.nextInt(100))
    val t22 = t11.clone();

    var time = System.nanoTime()
    mergeSort(t11.toList)
    println("czas quicksortu: " + (System.nanoTime() - time) / 10000)

    time = System.nanoTime()
    mergeSortParallel(t22.toList)
    println("czas quicksortu rownolegle: " + (System.nanoTime() - time) / 10000)

    //testy dla QuickSort 100000
    println("\nQuickSort 100000 elementow")

    val t1 = Array.fill(100000)(scala.util.Random.nextInt(100))
    val t2 = t1.clone();

    time = System.nanoTime()
    quicksort(t1)
    println("czas quicksortu: " + (System.nanoTime() - time) / 10000)

    time = System.nanoTime()
    quicksortParallel(t2)
    println("czas quicksortu rownolegle: " + (System.nanoTime() - time) / 10000)

    //testy dla ciagu fibonacciego 10 elementowego
    println("\nFibonacci 10 liczb")

    time = System.nanoTime()
    fib(10)
    println("czas fib " + (System.nanoTime() - time) / 10000)

    time = System.nanoTime()
    fibParallel(10)
    println("czas fib rownolegle " + (System.nanoTime() - time) / 10000)

    //testy dla ciagu fibonacciego 40 elementowego
    println("\nFibonacci 40 liczb")

    var turboTime = System.nanoTime()

    time = System.nanoTime()
    fib(40)
    println("czas fib " + (System.nanoTime() - time) / 10000)

    time = System.nanoTime()
    println(fibParallel(40))
    println("czas fib rownolegle " + (System.nanoTime() - time) / 10000)

    println("\nczas testow fib 40 elementow normalnie " + (System.nanoTime() - turboTime) / 10000)

    //testy dla ciagu fibonacciego korzystajc z zrownoleglenia
    var turboTime2 = System.nanoTime()

    val future1 = Future(fib(40))
    val future2 = Future(fibParallel(40))
    Await.result(future1, Duration.Inf)
    Await.result(future2, Duration.Inf)
    println("czas testow fib 40 elementow rownolegle " + (System.nanoTime() - turboTime2) / 10000)

  }

}

