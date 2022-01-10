import MaxTreeElement.*
import MergeSort.*
import SumMathSequence.*

object Tests {

  def executionTime[A](task: => A): Long =
    val t0 = System.nanoTime()
    task
    val t1 = System.nanoTime()
    t1 - t0

  def main(args: Array[String]): Unit = {

    var normalTime: Long = 0
    var parallelTime: Long = 0
    var timesRatio: Double = 0
    var triesNumber: Int = 100

    /*
    //max element in tree - variables
    var testTree: BT[Double] = Empty
    var treeDepth = 20
    var splitDepth = 6

    //max element in tree - tests
    for (i <- 0 until triesNumber)
      testTree = generateBTree(treeDepth)
      normalTime += executionTime(normalMaxElementInTree(testTree))
      parallelTime += executionTime(parallelMaxElementInTree(testTree, splitDepth))
    normalTime /= triesNumber
    parallelTime /= triesNumber
    timesRatio = normalTime.toDouble / parallelTime.toDouble

    //max element in tree - results
    println(s"number of tries : $triesNumber")
    println(s"tree depth  : $treeDepth")
    println(s"split depth : $splitDepth")
    println(s"normal time   : $normalTime")
    println(s"parallel time : $parallelTime")
    println(s"normal time / parallel time : $timesRatio")
    */
    /*
    //mergesort - variables
    var listToSort: List[Int] = Nil
    var listLength = 100000
    var splitNumber = 6

    normalTime = 0
    parallelTime = 0
    timesRatio = 0

    //mergesort - tests
    for (i <- 0 until triesNumber)
      listToSort = generateListToSort(listLength)
      normalTime += executionTime(normalMergeSort(listToSort))
      parallelTime += executionTime(parallelMergeSort(listToSort, splitNumber))
    normalTime /= triesNumber
    parallelTime /= triesNumber
    timesRatio = normalTime.toDouble / parallelTime.toDouble

    //mergesort - results
    println(s"number of tries : $triesNumber")
    println(s"list length  : $listLength")
    println(s"split number : $splitNumber")
    println(s"normal time   : $normalTime")
    println(s"parallel time : $parallelTime")
    println(s"normal time / parallel time : $timesRatio")
    */
    /*
    //sum elements in math sequence - variables
    var elementsToSum = 1000000
    var splitNumber = 6

    normalTime = 0
    parallelTime = 0
    timesRatio = 0

    //sum elements in math sequence - tests
    for (i <- 0 until triesNumber)
      normalTime += executionTime(normalSumMathSequence(elementsToSum))
      parallelTime += executionTime(parallelSumMathSequence(elementsToSum, splitNumber))
    normalTime /= triesNumber
    parallelTime /= triesNumber
    timesRatio = normalTime.toDouble / parallelTime.toDouble

    //sum elements in math sequence - results
    println(s"number of tries : $triesNumber")
    println(s"elements to sum  : $elementsToSum")
    println(s"split number : $splitNumber")
    println(s"normal time   : $normalTime")
    println(s"parallel time : $parallelTime")
    println(s"normal time / parallel time : $timesRatio")
    */
  }
}
