package Test

import QuickSort.QuickSort.*
import Fibonacci.Fibonacci.*
import SumOfArray.SumOfArray.*
import SumOfTree.SumOfTree.*
import scala.util.Random

object TimeTest extends App {

  def timeInMilliSeconds[A](body: => A): Unit =
  {
    val t0 = System.currentTimeMillis()
    body
    val t1 = System.currentTimeMillis()
    println("Time: " + (t1 - t0) + " ms")
  }

  def timeInNanoSeconds[A](body: => A): Unit =
  {
    val t0 = System.nanoTime()
    body
    val t1 = System.nanoTime()
    println("Time: " + (t1 - t0) + " ns")
  }

  def sumOfArrayTest(): Unit =
  {
    val random: Random.type = scala.util.Random

    val arr_10k = Array.fill(10000)(random.nextInt(1000000)-500000)
    val arr_100k = Array.fill(100000)(random.nextInt(1000000)-500000)
    val arr_1m = Array.fill(1000000)(random.nextInt(1000000)-500000)
    val arr_10m = Array.fill(10000000)(random.nextInt(1000000)-500000)

    println("*********************************** Sum of array ***********************************")

    println("10 000 elements: ")
    print("Normal: ")
    timeInNanoSeconds(sum(arr_10k))
    print("Future: ")
    sumFuture(arr_10k.clone())
    timeInNanoSeconds(sumFuture(arr_10k))
    print("Parallel: ")
    sumParallel(arr_10k.clone())
    timeInNanoSeconds(sumParallel(arr_10k))

    println()

    println("100 000 elements: ")
    print("Normal: ")
    timeInNanoSeconds(sum(arr_100k))
    print("Future: ")
    sumFuture(arr_100k.clone())
    timeInNanoSeconds(sumFuture(arr_100k))
    print("Parallel: ")
    sumParallel(arr_100k.clone())
    timeInNanoSeconds(sumParallel(arr_100k))

    println()

    println("1 000 000 elements: ")
    print("Normal: ")
    timeInNanoSeconds(sum(arr_1m))
    print("Future: ")
    sumFuture(arr_1m.clone())
    timeInNanoSeconds(sumFuture(arr_1m))
    print("Parallel: ")
    sumParallel(arr_1m.clone())
    timeInNanoSeconds(sumParallel(arr_1m))

    println()

    println("10 000 000 elements: ")
    print("Normal: ")
    timeInNanoSeconds(sum(arr_10m))
    print("Future: ")
    sumFuture(arr_10m.clone())
    timeInNanoSeconds(sumFuture(arr_10m))
    print("Parallel: ")
    sumParallel(arr_10m.clone())
    timeInNanoSeconds(sumParallel(arr_10m))

  }

  def quickSortTest(): Unit =
  {
    val random: Random.type = scala.util.Random

    val arr_10k = Array.fill(10000)(random.nextInt(1000000))
    val arr_100k = Array.fill(100000)(random.nextInt(1000000))
    val arr_1m = Array.fill(1000000)(random.nextInt(1000000))
    val arr_10m = Array.fill(10000000)(random.nextInt(1000000))

    println("*********************************** QuickSort ***********************************")

    println("10 000 elements: ")
    print("Normal quick sort: ")
    timeInMilliSeconds(quicksort(arr_10k.clone()))
    print("Future quick sort: ")
    quicksortFuture(arr_10k.clone())
    timeInMilliSeconds(quicksortFuture(arr_10k.clone()))
    print("Parallel quick sort: ")
    quicksortParallel(arr_10k.clone())
    timeInMilliSeconds(quicksortParallel(arr_10k.clone()))

    println()

    println("100 000 elements: ")
    print("Normal quick sort: ")
    timeInMilliSeconds(quicksort(arr_100k.clone()))
    print("Future quick sort: ")
    quicksortFuture(arr_100k.clone())
    timeInMilliSeconds(quicksortFuture(arr_100k.clone()))
    print("Parallel quick sort: ")
    quicksortParallel(arr_100k.clone())
    timeInMilliSeconds(quicksortParallel(arr_100k.clone()))

    println()

    println("1 000 000 elements: ")
    print("Normal quick sort: ")
    timeInMilliSeconds(quicksort(arr_1m.clone()))
    print("Future quick sort: ")
    quicksortFuture(arr_1m.clone())
    timeInMilliSeconds(quicksortFuture(arr_1m.clone()))
    print("Parallel quick sort: ")
    quicksortParallel(arr_1m.clone())
    timeInMilliSeconds(quicksortParallel(arr_1m.clone()))

    println()

    println("10 000 000 elements: ")
    print("Normal quick sort: ")
    timeInMilliSeconds(quicksort(arr_10m.clone()))
    print("Future quick sort: ")
    quicksortFuture(arr_10m.clone())
    timeInMilliSeconds(quicksortFuture(arr_10m.clone()))
    print("Parallel quick sort: ")
    quicksortParallel(arr_10m.clone())
    timeInMilliSeconds(quicksortParallel(arr_10m.clone()))
  }

  def fibonacciTest(): Unit =
  {
    println("*********************************** Fibonacci ***********************************")

    println("For 10, Threshold = 5")
    print("Normal: ")
    timeInNanoSeconds(fibonacci(10))
    print("Future: ")
    fibonacciFuture(10, 5)
    timeInNanoSeconds(fibonacciFuture(10, 5))
    print("Parallel: ")
    fibonacciParallel(10, 5)
    timeInNanoSeconds(fibonacciParallel(10, 5))

    println()

    println("For 20, Threshold = 10")
    print("Normal: ")
    timeInNanoSeconds(fibonacci(20))
    print("Future: ")
    fibonacciFuture(20, 10)
    timeInNanoSeconds(fibonacciFuture(20, 10))
    print("Parallel: ")
    fibonacciParallel(20, 10)
    timeInNanoSeconds(fibonacciParallel(20, 10))

    println()

    println("For 30, Threshold = 21")
    print("Normal: ")
    timeInNanoSeconds(fibonacci(30))
    print("Future: ")
    fibonacciFuture(30, 21)
    timeInNanoSeconds(fibonacciFuture(30, 21))
    print("Parallel: ")
    fibonacciParallel(30, 21)
    timeInNanoSeconds(fibonacciParallel(30, 21))

    println()

    println("For 30, Threshold = 28")
    print("Normal: ")
    timeInNanoSeconds(fibonacci(30))
    print("Future: ")
    fibonacciFuture(30, 28)
    timeInNanoSeconds(fibonacciFuture(30, 28))
    print("Parallel: ")
    fibonacciParallel(30, 28)
    timeInNanoSeconds(fibonacciParallel(30, 28))

    println()

    println("For 40, Threshold = 28")
    print("Normal: ")
    timeInNanoSeconds(fibonacci(40))
    print("Future: ")
    fibonacciFuture(40, 28)
    timeInNanoSeconds(fibonacciFuture(40, 28))
    print("Parallel: ")
    fibonacciParallel(40, 28)
    timeInNanoSeconds(fibonacciParallel(40, 28))

    println()

    println("For 40, Threshold = 39")
    print("Normal: ")
    timeInNanoSeconds(fibonacci(40))
    print("Future: ")
    fibonacciFuture(40, 39)
    timeInNanoSeconds(fibonacciFuture(40, 39))
    print("Parallel: ")
    fibonacciParallel(40, 39)
    timeInNanoSeconds(fibonacciParallel(40, 39))
  }

  def sumOfTreeTest(): Unit =
  {
    println("*********************************** Sum of tree nodes ***********************************")
    val tree_3 = generateTree(3)
    val tree_10 = generateTree(10)
    val tree_15 = generateTree(15)
    val tree_20 = generateTree(20)

    println("Depth = 3, Threshold = 1")
    print("Normal: ")
    timeInNanoSeconds(treeSum(tree_3))
    print("Future: ")
    treeSumFuture(tree_3, 1)
    timeInNanoSeconds(treeSumFuture(tree_3, 1))
    print("Parallel: ")
    treeSumParallel(tree_3, 1)
    timeInNanoSeconds(treeSumParallel(tree_3, 1))

    println()

    println("Depth = 3, Threshold = 2")
    print("Normal: ")
    timeInNanoSeconds(treeSum(tree_3))
    print("Future: ")
    treeSumFuture(tree_3, 2)
    timeInNanoSeconds(treeSumFuture(tree_3, 2))
    print("Parallel: ")
    treeSumParallel(tree_3, 2)
    timeInNanoSeconds(treeSumParallel(tree_3, 2))

    println()

    println("Depth = 10, Threshold = 1")
    print("Normal: ")
    timeInNanoSeconds(treeSum(tree_10))
    print("Future: ")
    treeSumFuture(tree_10, 1)
    timeInNanoSeconds(treeSumFuture(tree_10, 1))
    print("Parallel: ")
    treeSumParallel(tree_10, 1)
    timeInNanoSeconds(treeSumParallel(tree_10, 1))

    println()

    println("Depth = 10, Threshold = 2")
    print("Normal: ")
    timeInNanoSeconds(treeSum(tree_10))
    print("Future: ")
    treeSumFuture(tree_10, 2)
    timeInNanoSeconds(treeSumFuture(tree_10, 2))
    print("Parallel: ")
    treeSumParallel(tree_10, 2)
    timeInNanoSeconds(treeSumParallel(tree_10, 2))

    println()

    println("Depth = 10, Threshold = 5")
    print("Normal: ")
    timeInNanoSeconds(treeSum(tree_10))
    print("Future: ")
    treeSumFuture(tree_10, 5)
    timeInNanoSeconds(treeSumFuture(tree_10, 5))
    print("Parallel: ")
    treeSumParallel(tree_10, 5)
    timeInNanoSeconds(treeSumParallel(tree_10, 5))

    println()

    println("Depth = 10, Threshold = 8")
    print("Normal: ")
    timeInNanoSeconds(treeSum(tree_10))
    print("Future: ")
    treeSumFuture(tree_10, 8)
    timeInNanoSeconds(treeSumFuture(tree_10, 8))
    print("Parallel: ")
    treeSumParallel(tree_10, 8)
    timeInNanoSeconds(treeSumParallel(tree_10, 8))

    println()

    println("Depth = 15, Threshold = 1")
    print("Normal: ")
    timeInNanoSeconds(treeSum(tree_15))
    print("Future: ")
    treeSumFuture(tree_15, 1)
    timeInNanoSeconds(treeSumFuture(tree_15, 1))
    print("Parallel: ")
    treeSumParallel(tree_15, 1)
    timeInNanoSeconds(treeSumParallel(tree_15, 1))

    println()

    println("Depth = 15, Threshold = 2")
    print("Normal: ")
    timeInNanoSeconds(treeSum(tree_15))
    print("Future: ")
    treeSumFuture(tree_15, 2)
    timeInNanoSeconds(treeSumFuture(tree_15, 2))
    print("Parallel: ")
    treeSumParallel(tree_15, 2)
    timeInNanoSeconds(treeSumParallel(tree_15, 2))

    println()

    println("Depth = 15, Threshold = 5")
    print("Normal: ")
    timeInNanoSeconds(treeSum(tree_15))
    print("Future: ")
    treeSumFuture(tree_15, 5)
    timeInNanoSeconds(treeSumFuture(tree_15, 5))
    print("Parallel: ")
    treeSumParallel(tree_15, 5)
    timeInNanoSeconds(treeSumParallel(tree_15, 5))

    println()

    println("Depth = 20, Threshold = 1")
    print("Normal: ")
    timeInNanoSeconds(treeSum(tree_20))
    print("Future: ")
    treeSumFuture(tree_20, 1)
    timeInNanoSeconds(treeSumFuture(tree_20, 1))
    print("Parallel: ")
    treeSumParallel(tree_20, 1)
    timeInNanoSeconds(treeSumParallel(tree_20, 1))

    println()

    println("Depth = 20, Threshold = 2")
    print("Normal: ")
    timeInNanoSeconds(treeSum(tree_20))
    print("Future: ")
    treeSumFuture(tree_20, 2)
    timeInNanoSeconds(treeSumFuture(tree_20, 2))
    print("Parallel: ")
    treeSumParallel(tree_20, 2)
    timeInNanoSeconds(treeSumParallel(tree_20, 2))

    println()

    println("Depth = 20, Threshold = 5")
    print("Normal: ")
    timeInNanoSeconds(treeSum(tree_20))
    print("Future: ")
    treeSumFuture(tree_20, 5)
    timeInNanoSeconds(treeSumFuture(tree_20, 5))
    print("Parallel: ")
    treeSumParallel(tree_20, 5)
    timeInNanoSeconds(treeSumParallel(tree_20, 5))

  }

  quickSortTest()
  //fibonacciTest()
  //sumOfArrayTest()
  //sumOfTreeTest()
}