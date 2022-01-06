import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}
import scala.util.Random
import scala.collection.mutable.ListBuffer
import scala.math.max

object lista7 {

  //Searching for prime numbers
  def primeNumbers(n: Int, from: Int = 1): List[Int] = {
    val list = ListBuffer[Int]()
    for (i <- from to n) {
      if (i == 2)
        list += 2
      else {
        var exists = false
        var flag = false
        while (!flag) {
          for (j <- 2 until i - 1) {
            if (i % j == 0) {
              exists = true
              flag = true
            }
          }
          flag = true
        }
        if (!exists) {
          list += i
        }
      }
    }
    list.toList
  }

  def futurePrimeNumbers(n: Int): List[Int] = {
    val first = Future {
      val result = primeNumbers(n / 3)
      result
    }
    val second = Future {
      val result = primeNumbers(n / 3 * 2, n / 3)
      result
    }
    val third = Future {
      val result = primeNumbers(n / 4 * 3, n / 3 * 2)
      result
    }

    val forth = Future {
      val result = primeNumbers(n, n / 4 * 3)
      result
    }

    Await.result(first, Duration.Inf) ::: Await.result(second, Duration.Inf) :::
      Await.result(third, Duration.Inf) ::: Await.result(forth, Duration.Inf)
  }

  //QuickSort
  def swap[A](tab: Array[A])(i: Int)(j: Int): Unit = {
    val temp = tab(i)
    tab(i) = tab(j)
    tab(j) = temp
  }

  def choosePivot[A](tab: Array[A])(m: Int)(n: Int): A =
    tab((m + n) / 2)

  def partition(tab: Array[Int])(left: Int)(right: Int): (Int, Int) = {
    var i = left
    var j = right
    val pivot = choosePivot(tab)(left)(right)
    while (i <= j) {
      while (tab(i) < pivot) i += 1
      while (pivot < tab(j)) j -= 1
      if (i <= j) {
        swap(tab)(i)(j)
        i += 1
        j -= 1
      }
    }
    (i, j)
  }

  def quick(tab: Array[Int])(left: Int)(right: Int): Unit = {
    if (left < right) {
      val (i, j) = partition(tab)(left)(right)
      if (j - left < right - i) {
        quick(tab)(left)(j)
        quick(tab)(i)(right)
      }
      else {
        quick(tab)(i)(right)
        quick(tab)(left)(j)
      }
    }
  }

  def quickFuture(tab: Array[Int])(left: Int)(right: Int): Unit = {
    if (left < right) {
      val (i, j) = partition(tab)(left)(right)
      if (j - left < right - i) {
        val fut1 = Future(quick(tab)(left)(j))
        val fut2 = Future(quick(tab)(i)(right))
        Await.result(fut1, Duration.Inf)
        Await.result(fut2, Duration.Inf)
      }
      else {
        val fut1 = Future(quick(tab)(i)(right))
        val fut2 = Future(quick(tab)(left)(j))
        Await.result(fut1, Duration.Inf)
        Await.result(fut2, Duration.Inf)
      }
    }
  }

  def quickSort(tab: Array[Int]): Unit =
    quick(tab)(0)(tab.length - 1)

  def quickSortFuture(tab: Array[Int]): Unit =
    quickFuture(tab)(0)(tab.length - 1)


  //Knapsack problem

  def knapsack(availableSpace: Int, weight: Array[Int], value: Array[Int], numberOfElem: Int): Int = {

    if (numberOfElem == 0 || availableSpace == 0)
      return 0
    if (weight(numberOfElem - 1) > availableSpace) {
      knapsack(availableSpace, weight, value, numberOfElem - 1)
    } else {
      val element1 = value(numberOfElem - 1) + knapsack(availableSpace - weight(numberOfElem - 1), weight, value, numberOfElem - 1)
      val element2 = knapsack(availableSpace, weight, value, numberOfElem - 1)

      max(element1, element2)
    }
  }

  def knapsackFuture(availableSpace: Int, weight: Array[Int], value: Array[Int], numberOfElem: Int): Int = {

    if (numberOfElem == 0 || availableSpace == 0)
      return 0
    if (weight(numberOfElem - 1) > availableSpace) {
      knapsack(availableSpace, weight, value, numberOfElem - 1)
    } else {
      val fut1 = Future(knapsack(value(numberOfElem - 1) + availableSpace - weight(numberOfElem - 1), weight, value, numberOfElem - 1))
      val fut2 = Future(knapsack(availableSpace, weight, value, numberOfElem - 1))

      val xd = Await.result(fut1, Duration.Inf)
      val xd1 = Await.result(fut2, Duration.Inf)

      if xd > xd1 then xd
      else xd1
    }
  }

  //Odmierzanie czasu
  def timeMeasureNanoSeconds[A](block: => A): Unit = {
    val t0 = System.nanoTime()
    block
    val t1 = System.nanoTime()
    println("Total time: " + (t1 - t0) + "ns")
  }

  def knapsackTest(): Unit = {

    val oneThousandElem = 1000
    val firstWeightList = Array.fill(oneThousandElem)(scala.util.Random.nextInt(10000))
    val firstValueList = Array.fill(oneThousandElem)(scala.util.Random.nextInt(10000))

    val fiveHundredElem = 500
    val secondWeightList = Array.fill(fiveHundredElem)(scala.util.Random.nextInt(10000))
    val secondValueList = Array.fill(fiveHundredElem)(scala.util.Random.nextInt(10000))

    val oneHundredElem = 100
    val thirdWeightList = Array.fill(oneHundredElem)(scala.util.Random.nextInt(10000))
    val thirdValueList = Array.fill(oneHundredElem)(scala.util.Random.nextInt(10000))

    val tenElem = 10
    val fourthWeightList = Array.fill(tenElem)(scala.util.Random.nextInt(10000))
    val fourthValueList = Array.fill(tenElem)(scala.util.Random.nextInt(10000))


    println("Knapsack problem for 1000 elements")
    print("Normal Knapsack problem: ")
    timeMeasureNanoSeconds(knapsack(1000, firstWeightList, firstValueList, oneThousandElem))
    print("Future Knapsack problem: ")
    timeMeasureNanoSeconds(knapsackFuture(1000, firstWeightList, firstValueList, oneThousandElem))
    println()

    println("Knapsack problem for 500 elements")
    print("Normal Knapsack problem: ")
    timeMeasureNanoSeconds(knapsack(1000, secondWeightList, secondValueList, fiveHundredElem))
    print("Future Knapsack problem: ")
    timeMeasureNanoSeconds(knapsackFuture(1000, secondWeightList, secondValueList, fiveHundredElem))
    println()

    println("Knapsack problem for 100 elements")
    print("Normal Knapsack problem: ")
    timeMeasureNanoSeconds(knapsack(1000, thirdWeightList, thirdValueList, oneHundredElem))
    print("Future Knapsack problem: ")
    timeMeasureNanoSeconds(knapsackFuture(1000, thirdWeightList, thirdValueList, oneHundredElem))
    println()

    println("Knapsack problem for 10 elements")
    print("Normal Knapsack problem: ")
    timeMeasureNanoSeconds(knapsack(1000, fourthWeightList, fourthValueList, tenElem))
    print("Future Knapsack problem: ")
    timeMeasureNanoSeconds(knapsackFuture(1000, fourthWeightList, fourthValueList, tenElem))
    println()


  }

  def primeNumberTimeTests(): Unit = {

    println("Searching for 100 first prime numbers")
    print("Normal searching: ")
    timeMeasureNanoSeconds(primeNumbers(100))
    print("Future searching: ")
    timeMeasureNanoSeconds(futurePrimeNumbers(100))
    println()

    println("Searching for 1000 first prime numbers")
    print("Normal searching: ")
    timeMeasureNanoSeconds(primeNumbers(1000))
    print("Future searching: ")
    timeMeasureNanoSeconds(futurePrimeNumbers(1000))
    println()

    println("Searching for 10000 first prime numbers")
    print("Normal searching: ")
    timeMeasureNanoSeconds(primeNumbers(10000))
    print("Future searching: ")
    timeMeasureNanoSeconds(futurePrimeNumbers(10000))
    println()

    println("Searching for 50000 first prime numbers")
    print("Normal searching: ")
    timeMeasureNanoSeconds(primeNumbers(50000))
    print("Future searching: ")
    timeMeasureNanoSeconds(futurePrimeNumbers(50000))
    println()

  }

  //Próbka testowa QuickSort
  def quickSortTimeTests(): Unit = {

    val tenThousandElements = Array.fill(10000)(scala.util.Random.nextInt(1000000))
    val hundredThousandElements = Array.fill(100000)(scala.util.Random.nextInt(1000000))
    val millionElements = Array.fill(1000000)(scala.util.Random.nextInt(1000000))
    val tenMillionElements = Array.fill(10000000)(scala.util.Random.nextInt(1000000))

    println("QuickSort array with 10000 (ten thousand) elements")
    print("Normal quick sort: ")
    timeMeasureNanoSeconds(quickSort(tenThousandElements.clone()))
    print("Future quick sort: ")
    timeMeasureNanoSeconds(quickSortFuture(tenThousandElements.clone()))
    println()

    println("QuickSort array with 100000 (hundred thousand) elements")
    print("Normal quick sort: ")
    timeMeasureNanoSeconds(quickSort(hundredThousandElements.clone()))
    print("Future quick sort: ")
    timeMeasureNanoSeconds(quickSortFuture(hundredThousandElements.clone()))
    println()

    println("QuickSort array with 1000000 (million) elements")
    print("Normal quick sort: ")
    timeMeasureNanoSeconds(quickSort(millionElements.clone()))
    print("Future quick sort: ")
    timeMeasureNanoSeconds(quickSortFuture(millionElements.clone()))
    println()

    println("QuickSort array with 10000000 (ten million) elements")
    print("Normal quick sort: ")
    timeMeasureNanoSeconds(quickSort(tenMillionElements.clone()))
    print("Future quick sort: ")
    timeMeasureNanoSeconds(quickSort(tenMillionElements.clone()))
    println()
  }


  def main(args: Array[String]): Unit = {
    println("QuickSort TEST")
    quickSortTimeTests()
    println()
    println("Prime numbers TEST")
    primeNumberTimeTests()
    println()
    println("Knapsack problem")
    knapsackTest()
  }

}