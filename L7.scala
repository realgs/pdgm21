import Fibonacci.*
import ListComparer.*
import QuickSort.*

import java.security.cert.PolicyQualifierInfo
import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration

def countTimeNs[T](task: => T): Long = {
  val start = System.nanoTime();
  task
  val end = System.nanoTime()

  (end - start)
}

def printTime(time: Long): Unit = {
  val nano = time
  val mili = time / 1000000

  println(s"Mineło: ${nano} nanosekund, ${mili} milisekund")
  println()
}


object L7 {
  def main(args: Array[String]): Unit = {





    print("Fibonacci: ")
    printTime(countTimeNs(fibonacci(10)))
    print("FibonacciParallel (6): ")
    printTime(countTimeNs(fibonacciParallel(10,6)))
    print("FibonacciParallel (18): ")
    printTime(countTimeNs(fibonacciParallel(10,18)))
    print("FibonacciParallel (2): ")
    printTime(countTimeNs(fibonacciParallel(10,2)))


    print("Fibonacci: ")
    printTime(countTimeNs(fibonacci(45)))
    print("FibonacciParallel (6): ")
    printTime(countTimeNs(fibonacciParallel(45,6)))
    print("FibonacciParallel (18): ")
    printTime(countTimeNs(fibonacciParallel(45,18)))
    print("FibonacciParallel (2): ")
    printTime(countTimeNs(fibonacciParallel(45,2)))


    println()
    println()
    println()

    val listSmall1 = createList(1000,100_000)
    val listSmall2 = createList(1000,100_000)


    val listBig1 = createList(10,50_000_000)
    val listBig2 = createList(10,50_000_000)

    print("isSumGreater: ")
    printTime(countTimeNs(isSumGreater(listSmall1,listSmall2)))
    print("isSumGreaterParallel: ")
    printTime(countTimeNs(isSumGreaterParallel(listSmall1,listSmall2)))

    print("isSumGreater: ")
    printTime(countTimeNs(isSumGreater(listBig1,listBig2)))
    print("isSumGreaterParallel: ")
    printTime(countTimeNs(isSumGreaterParallel(listBig1,listBig2)))

    println()
    println()
    println()

    val listSmall = List(1,2,3,-1,3,12,41,1,24,1,4,1,41,6,27,345,32)
    val listBig = createList(1000, 50)

    print("quicksort: ")
    printTime(countTimeNs(quickSort(listSmall)))
    print("quicksortParallel: ")
    printTime(countTimeNs(quickSortParallel(listSmall)))


    print("quicksort: ")
    printTime(countTimeNs(quickSort(listBig)))
    print("quicksortParallel: ")
    printTime(countTimeNs(quickSortParallel(listBig)))


  }
}
