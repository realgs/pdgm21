import scala.concurrent.{Await, Future}
import scala.concurrent.duration.Duration
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.Random


object lab7 {

  //QuickSort

  def swap(array : Array[Int], i : Int, j : Int) : Unit = {
    val help = array(i)
    array(i) = array(j)
    array(j) = help
  }

  def choosePivot(array : Array[Int], l : Int, r : Int) : Int = {
    array((l + r) / 2)
  }

  def partition(subArray : Array[Int], left : Int, right : Int) : (Int, Int) = {
    var i = left
    var j = right
    val pivot = choosePivot(subArray, left, right)

    while (i <= j){
      while (subArray(i) < pivot){
        i += 1
      }
      while (pivot < subArray(j)){
        j -= 1
      }
      if (i <= j){
        swap(subArray, i, j)
        i += 1
        j -= 1
      }
    }
    (i, j)
  }

  def quickSort(array : Array[Int]) : Unit = {
    def quick(array : Array[Int], left : Int, right : Int) : Unit = {
      if (left < right) {
        val (i, j) = partition(array, left, right)
        if (j - left < right - i) {
          quick(array, left, j)
          quick(array, i, right)
        }
        else {
          quick(array, i, right)
          quick(array, left, j)
        }
      }
    }
    quick(array, 0, array.length - 1)
  }

  def quickSortParallel(array : Array[Int]) : Unit = {
    def quickPar(array : Array[Int], left : Int, right : Int) : Unit = {
      if (left < right){
        val (i, j) = partition(array, left, right)
        if (j - left < right - i){
          val quick1 = Future(quickPar(array, left, j))
          val quick2 = Future(quickPar(array, i, right))
          Await.result(quick1, Duration.Inf)
          Await.result(quick2, Duration.Inf)
        }
        else {
          val quick1 = Future(quickPar(array, i, right))
          val quick2 = Future(quickPar(array, left, j))
          Await.result(quick1, Duration.Inf)
          Await.result(quick2, Duration.Inf)
        }
      }
    }
    quickPar(array, 0, array.length - 1)
  }

  //Fibonacci

  def fibonacci(n : Int) : BigInt = {
    n match {
      case 0 => 0
      case 1 => 1
      case _ => fibonacci(n - 1) + fibonacci(n - 2)
    }
  }

  def fibonacciParallel(n : Int) : BigInt = {
    n match {
      case 0 => 0
      case 1 => 1
      case _ =>
        val fib1 = Future(fibonacci(n - 1))
        val fib2 = Future(fibonacci(n - 2))
        Await.result(fib1, Duration.Inf) + Await.result(fib2, Duration.Inf)
    }
  }

  //Multiply elements of array

  def multiply(array : Array[Int]) : BigInt = {
    var result = 0

    for (i <- 0 to array.length - 1){
      result *= array(i)
    }
    return result
  }

  def multiplyParallel(array : Array[Int]) : BigInt = {
    def multiplyParHelper(array : Array[Int], left : Int, right : Int) : BigInt = {
      var result = 0
      for (i <- left to right - 1){
        result *= array(i)
      }
      return result
    }
    val mul1 = Future(multiplyParHelper(array, 0, array.length / 4))
    val mul2 = Future(multiplyParHelper(array, array.length / 4, array.length / 2))
    val mul3 = Future(multiplyParHelper(array, array.length / 2, array.length * 3 / 4))
    val mul4 = Future(multiplyParHelper(array, array.length * 3 / 4, array.length))

    return Await.result(mul1, Duration.Inf) * Await.result(mul2, Duration.Inf) * Await.result(mul3, Duration.Inf) * Await.result(mul4, Duration.Inf)
  }

  def time[A](block : => A) = {
    val time0 = System.nanoTime()
    block
    val time1 = System.nanoTime()
    println("Time: " + (time1 - time0))
  }

  def main(args: Array[String]): Unit = {

    val a1 = Array.fill(100)(scala.util.Random.nextInt(1500) - 500)
    val a2 = Array.fill(1000)(scala.util.Random.nextInt(1500) - 500)
    val a3 = Array.fill(10000)(scala.util.Random.nextInt(1500) - 500)
    val a4 = Array.fill(100000)(scala.util.Random.nextInt(1500) - 500)
    val a5 = Array.fill(10000000)(scala.util.Random.nextInt(1500) - 500)

    println("QuickSort 100 - normal")
    time(quickSort(a1.clone()))
    println("QuickSort 100 - parallel")
    time(quickSortParallel(a1.clone()))
    println()

    println("QuickSort 1000 - normal")
    time(quickSort(a2.clone()))
    println("QuickSort 1000 - parallel")
    time(quickSortParallel(a2.clone()))
    println()

    println("QuickSort 10000 - normal")
    time(quickSort(a3.clone()))
    println("QuickSort 10000 - parallel")
    time(quickSortParallel(a3.clone()))
    println()

    println("Fibonacci 5 - normal")
    time(fibonacci(5))
    println("Fibonacci 5 - parallel")
    time(fibonacciParallel(5))
    println()

    println("Fibonacci 10 - normal")
    time(fibonacci(10))
    println("Fibonacci 10 - parallel")
    time(fibonacciParallel(10))
    println()

    println("Fibonacci 20 - normal")
    time(fibonacci(20))
    println("Fibonacci 20 - parallel")
    time(fibonacciParallel(20))
    println()

    println("Fibonacci 30 - normal")
    time(fibonacci(30))
    println("Fibonacci 30 - parallel")
    time(fibonacciParallel(30))
    println()

    println("Fibonacci 40 - normal")
    time(fibonacci(40))
    println("Fibonacci 40 - parallel")
    time(fibonacciParallel(40))
    println()

    println("Multiply Array Elements 100 - normal")
    time(multiply(a1.clone()))
    println("Multiply Array Elements 100 - parallel")
    time(multiplyParallel(a1.clone()))
    println()

    println("Multiply Array Elements 1000 - normal")
    time(multiply(a2.clone()))
    println("Multiply Array Elements 1000 - parallel")
    time(multiplyParallel(a2.clone()))
    println()

    println("Multiply Array Elements 10000 - normal")
    time(multiply(a3.clone()))
    println("Multiply Array Elements 10000 - parallel")
    time(multiplyParallel(a3.clone()))
    println()

    println("Multiply Array Elements 100000 - normal")
    time(multiply(a4.clone()))
    println("Multiply Array Elements 100000 - parallel")
    time(multiplyParallel(a4.clone()))
    println()

    println("Multiply Array Elements 10000000 - normal")
    time(multiply(a5.clone()))
    println("Multiply Array Elements 10000000 - parallel")
    time(multiplyParallel(a5.clone()))
    println()

  }

}

